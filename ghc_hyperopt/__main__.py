import logging
import sys
from argparse import ArgumentParser
from dataclasses import asdict
from pathlib import Path

import optuna
from optuna import Study, Trial

from ghc_hyperopt.cabal_build import CabalBuild, CabalBuildError
from ghc_hyperopt.ghc_config import GHCConfig, GHCConfigError
from ghc_hyperopt.process_info import ProcessError
from ghc_hyperopt.rts_config import RTSConfig, RTSConfigError
from ghc_hyperopt.tasty_bench import TastyBench, TastyBenchError
from ghc_hyperopt.tasty_config import TastyConfig
from ghc_hyperopt.utils import get_logger, pretty_print_json

logger = get_logger(__name__)


def objective(project_path: Path, component_name: str, artifact_dir: Path, trial: Trial) -> float:
    try:
        # Sample our configs
        ghc_config = GHCConfig.from_trial(trial)
        rts_config = RTSConfig.from_trial(trial)

        # Build the project
        build_info = CabalBuild.do(project_path, component_name, artifact_dir, ghc_config)

        # Add the build info to the trial
        trial.set_user_attr("build.component_name", build_info.component_name)
        for key in ["time_total", "mem_peak"]:
            trial.set_user_attr(f"build.process_info.{key}", getattr(build_info.process_info, key))

        # Get the location of the benchmark executable
        bench_executable_path = build_info.list_bin()

        # Run the benchmark
        bench_info = TastyBench.do(bench_executable_path, TastyConfig(), rts_config)

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
        return bench_info.process_info.time_total

    except Exception as e:
        error_name = e.__class__.__name__
        trial.set_user_attr("error.name", error_name)
        match e:
            case GHCConfigError():
                pass
            case RTSConfigError():
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
                logger.error("Unhandled category of error: %s", e)
                trial.set_user_attr("error.data", e.args)
                raise e

        logger.warning("Pruning trial due to GHCConfigError: %s", pretty_print_json(e.args[0]))
        raise optuna.exceptions.TrialPruned()


def main(project_path: Path, component_name: str, artifact_dir: Path) -> None:
    if not project_path.exists():
        raise FileNotFoundError(project_path)

    # Make sure the artifact directory exists
    artifact_dir.mkdir(parents=True, exist_ok=True)
    db_path = artifact_dir / "ghc_hyperopt.db"

    # Add stream handler of stdout to show the messages
    optuna.logging.get_logger("optuna").addHandler(logging.StreamHandler(sys.stdout))
    study: Study = optuna.create_study(
        study_name="ghc_hyperopt",
        direction="minimize",
        storage=optuna.storages.RDBStorage(
            url="sqlite:///" + db_path.as_posix(),
        ),
        load_if_exists=db_path.exists(),
    )
    study.optimize(
        lambda trial: objective(
            project_path,
            component_name,
            artifact_dir,
            trial,
        ),
        n_jobs=2,
        n_trials=2000,
    )
    print("Best params: ")
    print(study.best_params)


if __name__ == "__main__":
    parser = ArgumentParser()
    _ = parser.add_argument(
        "--project-path",
        type=Path,
        help="The path to the project to benchmark (e.g., FibHaskell).",
    )
    _ = parser.add_argument(
        "--component-name",
        type=str,
        help="The name of the component to benchmark, (e.g., bench:bench-fib).",
    )
    _ = parser.add_argument(
        "--artifact-dir",
        type=Path,
        help="The directory to store the artifacts.",
    )
    args = parser.parse_args()
    main(
        project_path=Path(args.project_path),
        component_name=args.component_name,
        artifact_dir=Path(args.artifact_dir),
    )
