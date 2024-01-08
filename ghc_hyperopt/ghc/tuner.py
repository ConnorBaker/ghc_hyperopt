import shutil
from pathlib import Path
from typing import final, overload

import optuna
import tqdm
from optuna import Study
from optuna.trial import FrozenTrial, Trial

from ghc_hyperopt.cabal_build import CabalBuild, CabalBuildError
from ghc_hyperopt.ghc.config import GhcConfig, GhcConfigError
from ghc_hyperopt.process_info import ProcessError
from ghc_hyperopt.rts_config import RtsConfig, RtsConfigError
from ghc_hyperopt.tasty.bench_suite import TastyBenchSuite
from ghc_hyperopt.tasty.error import (
    TastyBenchmarkParseError,
    TastyBenchSuiteRuntimeError,
)
from ghc_hyperopt.tasty_config import TastyConfig
from ghc_hyperopt.utils import OurBaseException, OurBaseModel, get_logger

logger = get_logger(__name__)


class GhcTunerMultipleBaselinesError(OurBaseException):
    """Multiple baselines found."""


# NOTE: We cannot have a copy of the study as a field because Pydantic can't infer a schema for it.


@final
class GhcTuner(OurBaseModel):
    """
    A class which performs tuning of GHC options.
    """

    project_path: Path
    """The path to the project to tune."""

    component_name: str
    """The name of the component to tune."""

    artifact_dir: Path
    """The directory to store artifacts in."""

    ghc_config: GhcConfig
    """The GHC configuration."""

    baseline_trial_number: int
    """The baseline trial."""

    @staticmethod
    def create_baseline(
        study: Study,
        project_path: Path,
        component_name: str,
        artifact_dir: Path,
    ) -> GhcTunerMultipleBaselinesError | TastyBenchmarkParseError | TastyBenchSuiteRuntimeError | FrozenTrial:
        """
        Create or get the baseline trial.
        """
        # Check if the baseline already exists
        baselines = [trial for trial in study.trials if trial.user_attrs.get("baseline", False)]

        match baselines:
            case []:
                logger.info("No baseline found, creating a new one")
                pass
            case [baseline]:
                return baseline
            case _:
                return GhcTunerMultipleBaselinesError("Multiple baselines found; delete the database and try again")

        trial = study.ask()
        trial.set_user_attr("baseline", True)

        build_info = GhcTuner._build(
            trial,
            project_path=project_path,
            component_name=component_name,
            artifact_dir=artifact_dir,
            ghc_config=GhcConfig(ghc_fixed_options={}, ghc_tuneable_options={}),
        )
        match build_info:
            case CabalBuildError():
                raise CabalBuildError("Failed to build baseline")
            case CabalBuild():
                pass

        bench_info = GhcTuner._bench(trial, build_info)
        match bench_info:
            case TastyBenchmarkParseError() | TastyBenchSuiteRuntimeError() as error:
                return error
            case TastyBenchSuite():
                pass

        # Remove the build directory
        shutil.rmtree(build_info.build_dir)

        # Return the total time taken
        frozen_trial = study.tell(
            trial,
            bench_info.process_info.time_total,
            state=optuna.trial.TrialState.COMPLETE,
        )

        tqdm.tqdm.write(f"Baseline trial finished with value: {frozen_trial.value}")
        return frozen_trial

    @overload
    @staticmethod
    def _build(
        trial: Trial,
        *,
        self: "GhcTuner",
        project_path: None = None,
        component_name: None = None,
        artifact_dir: None = None,
        ghc_config: None = None,
    ) -> CabalBuildError | CabalBuild: ...

    @overload
    @staticmethod
    def _build(
        trial: Trial,
        *,
        self: None = None,
        project_path: Path,
        component_name: str,
        artifact_dir: Path,
        ghc_config: GhcConfig,
    ) -> CabalBuildError | CabalBuild: ...

    @staticmethod
    def _build(
        trial: Trial,
        *,
        self: "None | GhcTuner" = None,
        project_path: None | Path = None,
        component_name: None | str = None,
        artifact_dir: None | Path = None,
        ghc_config: None | GhcConfig = None,
    ) -> CabalBuildError | CabalBuild:
        """
        Build the project with the given flags.
        """
        if self is not None:
            project_path = self.project_path
            component_name = self.component_name
            artifact_dir = self.artifact_dir
            ghc_config = self.ghc_config

        assert project_path is not None
        assert component_name is not None
        assert artifact_dir is not None
        assert ghc_config is not None

        # Build the project
        result = CabalBuild.do(
            project_path=project_path,
            component_name=component_name,
            artifact_dir=artifact_dir,
            flags=ghc_config.get_flags(trial),
        )

        match result:
            case CabalBuildError():
                return result
            case CabalBuild():
                pass

        # Add the build info to the trial
        trial.set_user_attr("build.component_name", result.component_name)
        for key in ["time_total", "mem_peak"]:
            trial.set_user_attr(f"build.process_info.{key}", getattr(result.process_info, key))

        return result

    @staticmethod
    def _bench(
        trial: Trial,
        build_info: CabalBuild,
    ) -> TastyBenchmarkParseError | TastyBenchSuiteRuntimeError | TastyBenchSuite:
        """
        Run the benchmarks.
        """
        # Run the benchmark
        # TODO: Should RTS be fixed in this way, or should there be a configuration we pass in?
        result = TastyBenchSuite.do(
            executable_path=build_info.executable_path,
            tasty_config=TastyConfig(),
            rts_config=RtsConfig(),
        )

        match result:
            case TastyBenchmarkParseError() | TastyBenchSuiteRuntimeError() as error:
                return error
            case TastyBenchSuite():
                pass

        # Add the overall benchmark info to the trial
        for key in ["time_total", "mem_peak"]:
            trial.set_user_attr(f"bench.process_info.{key}", getattr(result.process_info, key))

        # Add each benchmark to the trial
        for benchmark in result.benchmarks:
            name = benchmark.name
            for key, value in benchmark.model_dump(exclude={"none"}).items():
                trial.set_user_attr(f"bench.benchmark.{name}.{key}", value)

        return result

    @staticmethod
    def _prune(study: Study, trial: Trial, error: Exception) -> FrozenTrial:
        """
        Prune the trial.
        """
        error_name = error.__class__.__name__
        trial.set_user_attr("error.name", error_name)
        match error:
            case GhcConfigError():
                trial.set_user_attr("error.ghc", error.msg)
                logger.warning("Pruning trial due to %s: %s", error_name, error.msg)
            case RtsConfigError():
                trial.set_user_attr("error.rts", error.msg)
                logger.warning("Pruning trial due to %s: %s", error_name, error.msg)
            case CabalBuildError():
                trial.set_user_attr("error.build", error.msg)
                logger.warning("Pruning trial due to %s: %s", error_name, error.msg)
            case TastyBenchmarkParseError() | TastyBenchSuiteRuntimeError():
                trial.set_user_attr("error.bench", error.args[0])
                logger.warning("Pruning trial due to %s: %s", error_name, error.msg)
            case ProcessError():
                trial.set_user_attr("error.process", error.args[0])
                logger.warning("Pruning trial due to %s: %s", error_name, error.msg)
            case _:
                trial.set_user_attr("error.unknown", error.args)
                logger.error("Unknown category of error %s: %s", error_name, error)

        frozen_trial = study.tell(trial, values=None, state=optuna.trial.TrialState.PRUNED)
        tqdm.tqdm.write("Pruned trial")
        return frozen_trial

    def tune(self, study: Study) -> FrozenTrial:
        """
        Run one trial of the tuner.
        """
        trial = study.ask()

        build_info = GhcTuner._build(trial, self=self)
        match build_info:
            case CabalBuildError() as error:
                return self._prune(study, trial, error)
            case CabalBuild():
                pass

        bench_info = self._bench(trial, build_info)
        match bench_info:
            case TastyBenchmarkParseError() | TastyBenchSuiteRuntimeError() as error:
                return self._prune(study, trial, error)
            case TastyBenchSuite():
                pass

        # Remove the build directory
        shutil.rmtree(build_info.build_dir)

        # Return the total time taken
        frozen_trial = study.tell(trial, bench_info.process_info.time_total)

        tqdm.tqdm.write(f"Trial {frozen_trial.number} finished with value: {frozen_trial.value}")
        tqdm.tqdm.write(f"Best trial is {study.best_trial.number} with value: {study.best_trial.value}")

        return frozen_trial
