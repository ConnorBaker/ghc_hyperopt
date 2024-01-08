import logging
import sys
from argparse import Namespace
from collections.abc import Mapping, Sequence
from pathlib import Path
from typing import Any

import optuna
import tqdm
from optuna.study import Study, StudyDirection
from optuna.trial import Trial

from ghc_hyperopt.cli import get_arg_parser
from ghc_hyperopt.ghc.config import GhcConfig
from ghc_hyperopt.ghc.info import GhcInfo
from ghc_hyperopt.ghc.options import GhcOption, get_all_ghc_options, get_all_ghc_options_optuna_samplers
from ghc_hyperopt.ghc.options.questionable import GhcQuestionableOptions
from ghc_hyperopt.ghc.tuner import GhcTuner
from ghc_hyperopt.tasty.benchmark import TastyBenchmarkOptimizationChoices
from ghc_hyperopt.utils import SampleFn, get_logger

logger = get_logger(__name__)


def mk_ghc_config(args: Namespace) -> GhcConfig:
    # Get info about GHC to check support for options
    ghc_info = GhcInfo.get()

    # The fixed options will use their default values
    ghc_fixed_options: dict[str, GhcOption[Any]] = {}

    # The tuneable options will use the corresponding sampler
    ghc_tuneable_options: dict[str, tuple[GhcOption[Any], SampleFn[Trial, Any]]] = {}
    ghc_options_optuna_samplers: Mapping[str, SampleFn[Trial, Any]] = get_all_ghc_options_optuna_samplers()

    # We don't want to tune the questionable options by default
    questionable_option_names = GhcQuestionableOptions.__class_vars__

    # Map each option specified on the command line to its corresponding sampler or fixed value
    for name, option in get_all_ghc_options().items():
        # Make sure we have a corresponding sampler
        assert name in ghc_options_optuna_samplers

        # Skip the option if unsupported by the current GHC/target triple
        if not all(req.satisfied_by(ghc_info) for req in option.ghc_requirements):
            logger.info("Skipping %s because it is not supported by the current GHC/target triple", name)
            continue

        # Skip the option if it is a questionable option and we are not tuning them explicitly
        if getattr(args, f"tune_ghc_{name}", False) or (args.tune_ghc_all and name not in questionable_option_names):
            ghc_tuneable_options[name] = option, ghc_options_optuna_samplers[name]
        else:
            ghc_fixed_options[name] = option

    return GhcConfig(
        optimization=2,
        rtsopts="all",
        ghc_fixed_options=ghc_fixed_options,
        ghc_tuneable_options=ghc_tuneable_options,
    )


def mk_study(artifact_dir: Path, optimization_directions: Sequence[StudyDirection]) -> Study:
    # Make sure the artifact directory exists
    db_path = artifact_dir / "ghc_hyperopt.db"

    # Add stream handler of stdout to show the messages
    optuna.logging.get_logger("optuna").addHandler(logging.StreamHandler(sys.stdout))
    return optuna.create_study(
        study_name="ghc_hyperopt",
        directions=optimization_directions,
        storage=optuna.storages.RDBStorage(
            url="sqlite:///" + db_path.as_posix(),
        ),
        load_if_exists=True,
    )


def tune_ghc_options(args: Namespace) -> None:
    project_path: Path = args.project_path
    if not project_path.exists():
        raise FileNotFoundError(project_path)

    artifact_dir: Path = args.artifact_dir
    artifact_dir.mkdir(parents=True, exist_ok=True)

    component_name: str = args.component_name

    optimization_choices = TastyBenchmarkOptimizationChoices(
        time_mean=True,
        mem_allocated=False,
        mem_copied=False,
    )

    # TODO: This always creates a new baseline -- we want to query the GhcTuner to see if there's already a baseline
    # in the database.
    # Get our baseline
    logger.info("Running baseline build and benchmarks")
    (baseline_build_info, baseline_bench_info) = GhcTuner.run_baseline(
        project_path=project_path,
        component_name=component_name,
        artifact_dir=artifact_dir,
    )

    # Get the optimization directions (minimize for each optimization choice for each benchmark)
    optimization_directions = baseline_bench_info.get_optimization_directions(optimization_choices)

    ghc_config = mk_ghc_config(args)
    study = mk_study(artifact_dir, optimization_directions)
    baseline = GhcTuner.register_baseline(study, baseline_build_info, baseline_bench_info, optimization_choices)

    ghc_tuner = GhcTuner(
        project_path=args.project_path,
        component_name=args.component_name,
        artifact_dir=args.artifact_dir,
        ghc_config=ghc_config,
        optimization_choices=optimization_choices,
        baseline_trial_number=baseline.number,
    )

    # Don't use a progress bar if the log level is WARNING or lower
    range_fn = tqdm.trange if logger.getEffectiveLevel() > logging.WARNING else range

    logger.info("Beginning study")

    # Begin the study
    for _ in range_fn(10000):
        _ = ghc_tuner.tune(study)

    print("Number of finished trials: ", len(study.trials))
    print("Baseline trial: ")
    print(study.trials[baseline.number])
    print("Best trials: ")
    print(study.best_trials)


if __name__ == "__main__":
    arg_parser = get_arg_parser()
    args = arg_parser.parse_args()

    if args.tune_ghc:
        print("Tuning GHC options")
        tune_ghc_options(args)
    elif args.tune_rts:
        print("Tuning RTS options")
        raise NotImplementedError()
    else:
        raise RuntimeError("Unknown tuning kind")
