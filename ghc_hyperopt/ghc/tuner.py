from dataclasses import asdict, dataclass
from pathlib import Path
import shutil

import optuna
import tqdm
from optuna import Study

from ghc_hyperopt.cabal_build import CabalBuild, CabalBuildError
from ghc_hyperopt.ghc.config import GhcConfig, GhcConfigError
from ghc_hyperopt.process_info import ProcessError
from ghc_hyperopt.rts_config import RtsConfig, RtsConfigError
from ghc_hyperopt.tasty_bench import TastyBench, TastyBenchError
from ghc_hyperopt.tasty_config import TastyConfig
from ghc_hyperopt.utils import get_logger, pretty_print_json

logger = get_logger(__name__)


@dataclass(frozen=True, unsafe_hash=True, slots=True, kw_only=True)
class GhcTuner:
    """
    A class which performs tuning of GHC options.
    """

    study: Study
    """The Optuna study."""

    project_path: Path
    """The path to the project to tune."""

    component_name: str
    """The name of the component to tune."""

    artifact_dir: Path
    """The directory to store artifacts in."""

    ghc_config: GhcConfig
    """The GHC configuration."""

    def __post_init__(self) -> None:
        """Validate the tuner, raising an exception if it is invalid."""
        if not self.project_path.exists():
            raise FileNotFoundError(self.project_path)

        if not self.artifact_dir.exists():
            self.artifact_dir.mkdir(parents=True, exist_ok=True)

    def tune(self) -> None:
        """
        Run one trial of the tuner.
        """
        trial = self.study.ask()
        flags = self.ghc_config.get_flags(trial)

        try:
            # Build the project
            build_info = CabalBuild.do(
                project_path=self.project_path,
                component_name=self.component_name,
                artifact_dir=self.artifact_dir,
                flags=flags,
            )

            # Add the build info to the trial
            trial.set_user_attr("build.component_name", build_info.component_name)
            for key in ["time_total", "mem_peak"]:
                trial.set_user_attr(f"build.process_info.{key}", getattr(build_info.process_info, key))

            # Get the location of the benchmark executable
            bench_executable_path = build_info.list_bin()

            # Run the benchmark
            # TODO: Should RTS be fixed in this way, or should there be a configuration we pass in?
            bench_info = TastyBench.do(bench_executable_path, TastyConfig(), RtsConfig())

            # Remove the build directory
            shutil.rmtree(build_info.build_dir)

            # Add the overall benchmark info to the trial
            for key in ["time_total", "mem_peak"]:
                trial.set_user_attr(f"bench.process_info.{key}", getattr(bench_info.process_info, key))

            # Add each benchmark to the trial
            for benchmark in bench_info.benchmarks:
                name = benchmark.name
                for key, value in asdict(benchmark).items():
                    if key == "name":
                        continue
                    trial.set_user_attr(f"bench.benchmark.{name}.{key}", value)

            # Return the total time taken
            frozen_trial = self.study.tell(trial, bench_info.process_info.time_total)
            tqdm.tqdm.write(f"Trial {frozen_trial.number} finished with value: {frozen_trial.value}")
            tqdm.tqdm.write(f"Best trial is {self.study.best_trial.number} with value: {self.study.best_trial.value}")

        except Exception as e:
            error_name = e.__class__.__name__
            trial.set_user_attr("error.name", error_name)
            match e:
                # TODO: We should not have GHCConfigError or RTSConfigError ever occur, or cause pruning.
                # Known incompatabilities should be handled by correctly guarding the parameters.
                case GhcConfigError():
                    pass
                case RtsConfigError():
                    trial.set_user_attr("error.rts", e.broken_invariant)
                case CabalBuildError():
                    # TODO: Add actual arguments to each of these class of errors so we can have more
                    # descriptive error messages.
                    trial.set_user_attr("error.build", e.args[0])
                case TastyBenchError():
                    # TODO: Add actual arguments to each of these class of errors so we can have more
                    # descriptive error messages.
                    trial.set_user_attr("error.bench", e.args[0])
                case ProcessError():
                    # TODO: Add actual arguments to each of these class of errors so we can have more
                    # descriptive error messages.
                    trial.set_user_attr("error.process", e.args[0])
                case _:
                    logger.error("Unknown category of error %s: %s", error_name, e)
                    trial.set_user_attr("error.unknown", e.args)
                    raise e

            logger.warning("Pruning trial due to %s: %s", error_name, pretty_print_json(e.args[0]))
            _ = self.study.tell(trial, values=None, state=optuna.trial.TrialState.PRUNED)
            tqdm.tqdm.write("Pruned trial")
