import shutil
from collections.abc import Mapping, Sequence
from pathlib import Path
from typing import final, overload

import optuna
import tqdm
from optuna.study import Study
from optuna.trial import FrozenTrial, Trial

import wandb
from ghc_hyperopt.cabal_build import CabalBuild, CabalBuildError
from ghc_hyperopt.config import StudyDirection, objective
from ghc_hyperopt.ghc.config import GhcConfig, GhcConfigError
from ghc_hyperopt.process_info import ProcessError
from ghc_hyperopt.rts_config import RtsConfig, RtsConfigError
from ghc_hyperopt.tasty.bench_suite import TastyBenchSuite
from ghc_hyperopt.tasty.benchmark import (
    TastyBenchmark,
    TastyBenchmarkResult,
)
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

    optimization_choices: TastyBenchmark[bool]
    """The optimization choices."""

    optimization_directions: TastyBenchmark[StudyDirection]
    """The optimization directions for each metric."""

    optimization_weights: Mapping[str, TastyBenchmark[float]]
    """The weights for each metric."""

    baseline: TastyBenchSuite
    """The baseline trial."""

    @staticmethod
    def run_baseline(
        project_path: Path,
        component_name: str,
        artifact_dir: Path,
    ) -> tuple[CabalBuild, TastyBenchSuite]:
        match GhcTuner._build(
            flags=[
                "-rtsopts=all",
                # See https://github.com/Bodigrim/tasty-bench#troubleshooting
                "-with-rtsopts=-A32m",
                # See https://github.com/Bodigrim/tasty-bench#comparison-between-benchmarks
                "-fproc-alignment=64",
            ],
            project_path=project_path,
            component_name=component_name,
            artifact_dir=artifact_dir,
        ):
            case CabalBuildError() as error:
                raise error
            case CabalBuild() as build_info:
                pass

        match GhcTuner._bench(build_info):
            case TastyBenchmarkParseError() | TastyBenchSuiteRuntimeError() as error:
                shutil.rmtree(build_info.build_dir)
                raise error
            case TastyBenchSuite() as bench_info:
                pass

        # Remove the build directory
        shutil.rmtree(build_info.build_dir)

        return build_info, bench_info

    @staticmethod
    def register_baseline(
        study: Study,
        build_info: CabalBuild,
        bench_info: TastyBenchSuite,
        optimization_choices: TastyBenchmark[bool],
        optimization_weights: Mapping[str, TastyBenchmark[float]],
    ) -> FrozenTrial:
        """
        Register the baseline trial.
        """
        trial = study.ask()
        trial.set_user_attr("baseline", True)
        GhcTuner._register_build(trial, build_info)
        return GhcTuner._register_bench(
            study,
            trial,
            bench_info,
            bench_info,
            optimization_choices,
            optimization_weights,
        )

    @overload
    @staticmethod
    def _build(
        flags: Sequence[str],
        *,
        self: "GhcTuner",
        project_path: None = None,
        component_name: None = None,
        artifact_dir: None = None,
    ) -> CabalBuildError | CabalBuild: ...

    @overload
    @staticmethod
    def _build(
        flags: Sequence[str],
        *,
        self: None = None,
        project_path: Path,
        component_name: str,
        artifact_dir: Path,
    ) -> CabalBuildError | CabalBuild: ...

    @staticmethod
    def _build(
        flags: Sequence[str],
        *,
        self: "None | GhcTuner" = None,
        project_path: None | Path = None,
        component_name: None | str = None,
        artifact_dir: None | Path = None,
    ) -> CabalBuildError | CabalBuild:
        """
        Build the project with the given flags.
        """
        if self is not None:
            assert project_path is None
            project_path = self.project_path
            assert component_name is None
            component_name = self.component_name
            assert artifact_dir is None
            artifact_dir = self.artifact_dir

        assert project_path is not None
        assert component_name is not None
        assert artifact_dir is not None

        # Build the project
        return CabalBuild.do(
            project_path=project_path,
            component_name=component_name,
            artifact_dir=artifact_dir,
            flags=flags,
        )

    @staticmethod
    def _register_build(
        trial: Trial,
        build_info: CabalBuild,
    ) -> None:
        """
        Register the build info to the trial.
        """
        flattened = {
            ("build", "component_name"): build_info.component_name,
            ("build", "process_info", "time_total"): build_info.process_info.time_total,
            ("build", "process_info", "mem_peak"): build_info.process_info.mem_peak,
        }

        for key, value in flattened.items():
            trial.set_user_attr(".".join(key), value)

        wandb.log({"/".join(key): value for key, value in flattened.items()})

    @staticmethod
    def _bench(build_info: CabalBuild) -> TastyBenchmarkParseError | TastyBenchSuiteRuntimeError | TastyBenchSuite:
        """
        Run the benchmarks.
        """
        # Run the benchmark
        # TODO: Should RTS be fixed in this way, or should there be a configuration we pass in?
        return TastyBenchSuite.do(
            executable_path=build_info.executable_path,
            tasty_config=TastyConfig(),
            rts_config=RtsConfig(),
        )

    @staticmethod
    def _register_bench(  # noqa: PLR0917
        study: Study,
        trial: Trial,
        bench_info: TastyBenchSuite,
        baseline: TastyBenchSuite,
        optimization_choices: TastyBenchmark[bool],
        optimization_weights: Mapping[str, TastyBenchmark[float]],
    ) -> FrozenTrial:
        """
        Register the benchmark info to the trial.
        """

        # For logging to optuna and wandb
        flattened = {}

        # Pydantic lets us iterate over the fields of a model without converting sub-models to dicts
        for metric_name, metric_value in bench_info.process_info:
            flattened[("bench", "process_info", metric_name)] = metric_value

        for benchmark, baseline_benchmark in zip(bench_info.benchmarks, baseline.benchmarks, strict=True):
            key_prefix = ("bench", "benchmark")
            for metric_type, metrics in benchmark.zip_with(baseline_benchmark, TastyBenchmarkResult.from_raw):
                if metric_type == "name":
                    continue

                for metric_name, metric_value in metrics:
                    assert isinstance(metric_value, TastyBenchmarkResult)
                    key_suffix = "-".join([benchmark.name, metric_type, metric_name])
                    flattened[(*key_prefix, "raw", key_suffix)] = metric_value.raw
                    flattened[(*key_prefix, "percent_improvement", key_suffix)] = metric_value.percent_improvement

        # Add the flattened metrics to the trial
        for key, value in flattened.items():
            trial.set_user_attr(".".join(key), value)

        # Add the flattened metrics to wandb
        wandb.log({"/".join(key): value for key, value in flattened.items()})

        # Combine the percent improvements into a single value, weighted by the optimization choices
        objective_value: float = objective(
            bench_info,
            optimization_choices,
            optimization_weights,
            baseline,
        )

        # Return our metrics as a flat list for optuna
        frozen_trial = study.tell(trial, objective_value)

        tqdm.tqdm.write(
            f"Trial {frozen_trial.number} finished with improvements over baseline (%): {frozen_trial.values}"
        )
        tqdm.tqdm.write(f"Best trials are {' '.join(str(trial.number) for trial in study.best_trials)}")

        return frozen_trial

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
        trial.set_user_attr("baseline", False)

        match GhcTuner._build(self.ghc_config.get_flags(trial), self=self):
            case CabalBuildError() as error:
                return self._prune(study, trial, error)
            case CabalBuild() as build_info:
                self._register_build(trial, build_info)

        match self._bench(build_info):
            case TastyBenchmarkParseError() | TastyBenchSuiteRuntimeError() as error:
                shutil.rmtree(build_info.build_dir)
                return self._prune(study, trial, error)
            case TastyBenchSuite() as bench_info:
                shutil.rmtree(build_info.build_dir)
                return self._register_bench(
                    study,
                    trial,
                    bench_info,
                    self.baseline,
                    self.optimization_choices,
                    self.optimization_weights,
                )
