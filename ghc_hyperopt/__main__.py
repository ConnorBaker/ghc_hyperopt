import logging
import sys
from argparse import Namespace
from collections.abc import Mapping
from pathlib import Path
from typing import Any

import optuna
import tqdm
from optuna.trial import FrozenTrial, Trial

from ghc_hyperopt.cli import get_arg_parser
from ghc_hyperopt.ghc.config import GhcConfig
from ghc_hyperopt.ghc.info import GhcInfo
from ghc_hyperopt.ghc.options import GhcOption, get_all_ghc_options, get_all_ghc_options_optuna_samplers
from ghc_hyperopt.ghc.tuner import GhcTuner
from ghc_hyperopt.utils import SampleFn, get_logger

logger = get_logger(__name__)


def tune_ghc_options(args: Namespace) -> None:
    # Get info about GHC to check support for options
    ghc_info = GhcInfo.get()

    # The fixed options will use their default values
    ghc_fixed_options: dict[str, GhcOption[Any]] = {}

    # The tuneable options will use the corresponding sampler
    ghc_tuneable_options: dict[str, tuple[GhcOption[Any], SampleFn[Trial, Any]]] = {}
    ghc_options_optuna_samplers: Mapping[str, SampleFn[Trial, Any]] = get_all_ghc_options_optuna_samplers()

    # Map each option specified on the command line to its corresponding sampler or fixed value
    for name, option in get_all_ghc_options().items():
        # Make sure we have a corresponding sampler
        assert name in ghc_options_optuna_samplers

        # Skip the option if unsupported by the current GHC/target triple
        if not all(req.satisfied_by(ghc_info) for req in option.ghc_requirements):
            logger.info("Skipping %s because it is not supported by the current GHC/target triple", name)
            continue

        if getattr(args, f"tune_ghc_{name}", False) or args.tune_ghc_all:
            ghc_tuneable_options[name] = option, ghc_options_optuna_samplers[name]
        else:
            ghc_fixed_options[name] = option

    project_path: Path = args.project_path
    if not project_path.exists():
        raise FileNotFoundError(project_path)

    # Make sure the artifact directory exists
    artifact_dir: Path = args.artifact_dir
    artifact_dir.mkdir(parents=True, exist_ok=True)
    db_path = artifact_dir / "ghc_hyperopt.db"
    create_baseline = not db_path.exists()
    logger.info(
        "Database %s found, %s creating a new baseline",
        "was not" if create_baseline else "was",
        "now" if create_baseline else "not",
    )

    # Add stream handler of stdout to show the messages
    optuna.logging.get_logger("optuna").addHandler(logging.StreamHandler(sys.stdout))
    study = optuna.create_study(
        study_name="ghc_hyperopt",
        direction="minimize",
        storage=optuna.storages.RDBStorage(
            url="sqlite:///" + db_path.as_posix(),
        ),
        load_if_exists=True,
    )

    ghc_config = GhcConfig(
        optimization=2,
        rtsopts="all",
        ghc_fixed_options=ghc_fixed_options,
        ghc_tuneable_options=ghc_tuneable_options,
    )

    baseline = GhcTuner.create_baseline(
        study=study,
        project_path=project_path,
        component_name=args.component_name,
        artifact_dir=artifact_dir,
    )
    match baseline:
        case FrozenTrial():
            pass
        case error:
            raise error

    ghc_tuner = GhcTuner(
        project_path=args.project_path,
        component_name=args.component_name,
        artifact_dir=args.artifact_dir,
        ghc_config=ghc_config,
        baseline_trial_number=baseline.number,
    )

    # Don't use a progress bar if the log level is WARNING or lower
    range_fn = tqdm.trange if logger.getEffectiveLevel() > logging.WARNING else range

    # Begin the study
    for _ in range_fn(10):
        _ = ghc_tuner.tune(study)

    print("Number of finished trials: ", len(study.trials))
    print("Baseline trial: ")
    print(study.trials[baseline.number])
    print("Best trial: ")
    print(study.best_trial)
    print("Best params: ")
    print(study.best_params)


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
