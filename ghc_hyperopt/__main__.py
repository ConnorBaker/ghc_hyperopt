import logging
import sys
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

PROJECT_PATH = Path("/Users/connorbaker/Packages/ghc_hyperopt/FibHaskell")
COMPONENT_NAME = "bench:bench-fib"
TASTY_CONFIG = TastyConfig(
    csv=(PROJECT_PATH / "benchmarks.csv").as_posix(),
)

logger = get_logger(__name__)

# TODO: Much faster to re-run the bench with different RTS options than to re-build the project.
# Try to do multiple trials per build.


def objective(trial: Trial) -> float:
    try:
        # Sample our configs
        ghc_config = GHCConfig.from_trial(trial)
        rts_config = RTSConfig.from_trial(trial)

        # Build the project
        build_info = CabalBuild.do(PROJECT_PATH, COMPONENT_NAME, ghc_config)

        # Add the build info to the trial
        trial.set_user_attr("build.component_name", build_info.component_name)
        for key, value in asdict(build_info.process_info).items():
            trial.set_user_attr(f"build.process_info.{key}", value)

        # Get the location of the benchmark executable
        bench_executable_path = build_info.list_bin()

        # Run the benchmark
        bench_info = TastyBench.do(bench_executable_path, TASTY_CONFIG, rts_config)

        # Add the benchmark info to the trial
        trial.set_user_attr("bench.executable_path", bench_info.executable_path.as_posix())
        for key, value in asdict(bench_info.process_info).items():
            trial.set_user_attr(f"bench.process_info.{key}", value)

        # Add each benchmark to the trial
        for benchmark in bench_info.benchmarks.values():
            for key, value in asdict(benchmark).items():
                if key == "name":
                    continue
                trial.set_user_attr(f"bench.benchmark.{benchmark.name}.{key}", value)

        # Return the total time taken
        return bench_info.process_info.time_total

    except GHCConfigError as e:
        trial.set_user_attr("error.name", "GHCConfigError")
        trial.set_user_attr("error.data", e.args[0])
        logger.warning("Pruning trial due to GHCConfigError: %s", pretty_print_json(e.args[0]))
        raise optuna.exceptions.TrialPruned()

    except RTSConfigError as e:
        trial.set_user_attr("error.name", "RTSConfigError")
        trial.set_user_attr("error.data", e.args[0])
        logger.warning("Pruning trial due to RTSConfigError: %s", pretty_print_json(e.args[0]))
        raise optuna.exceptions.TrialPruned()

    except CabalBuildError as e:
        trial.set_user_attr("error.name", "CabalBuildError")
        trial.set_user_attr("error.data", e.args[0])
        logger.warning("Pruning trial due to CabalBuildError: %s", pretty_print_json(e.args[0]))
        raise optuna.exceptions.TrialPruned()

    except TastyBenchError as e:
        trial.set_user_attr("error.name", "TastyBenchError")
        trial.set_user_attr("error.data", e.args[0])
        logger.warning("Pruning trial due to TastyBenchError: %s", pretty_print_json(e.args[0]))
        raise optuna.exceptions.TrialPruned()

    except ProcessError as e:
        trial.set_user_attr("error.name", "ProcessError")
        trial.set_user_attr("error.data", e.args[0])
        logger.warning("Pruning trial due to ProcessError: %s", pretty_print_json(e.args[0]))
        raise optuna.exceptions.TrialPruned()


def main() -> None:
    # Add stream handler of stdout to show the messages
    optuna.logging.get_logger("optuna").addHandler(logging.StreamHandler(sys.stdout))
    study: Study = optuna.create_study(
        study_name="ghc_hyperopt",
        direction="minimize",
        storage=optuna.storages.RDBStorage(
            url="sqlite:///ghc_hyperopt.db",
        ),
        load_if_exists=True,
    )
    study.optimize(objective, n_trials=10000)
    print("Best params: ")
    print(study.best_params)


if __name__ == "__main__":
    main()
