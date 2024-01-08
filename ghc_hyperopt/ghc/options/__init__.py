from argparse import ArgumentParser
from collections.abc import Mapping
from typing import Any

from optuna.trial import Trial

from ghc_hyperopt.ghc.option import GhcOption
from ghc_hyperopt.ghc.options.inliner import GhcInlinerOptions, GhcInlinerOptionsOptunaSampler
from ghc_hyperopt.ghc.options.misc import GhcMiscOptions, GhcMiscOptionsOptunaSampler
from ghc_hyperopt.ghc.options.platform import GhcPlatformOptions, GhcPlatformOptionsOptunaSampler
from ghc_hyperopt.ghc.options.questionable import GhcQuestionableOptions, GhcQuestionableOptionsOptunaSampler
from ghc_hyperopt.ghc.options.simplifier import GhcSimplifierOptions, GhcSimplifierOptionsOptunaSampler
from ghc_hyperopt.ghc.options.specialiser import GhcSpecialiserOptions, GhcSpecialiserOptionsOptunaSampler
from ghc_hyperopt.ghc.options.stg_lift_lams import GhcStgLiftLamOptions, GhcStgLiftLamOptionsOptunaSampler
from ghc_hyperopt.utils import SampleFn

_option_groups = [
    GhcInlinerOptions,
    GhcMiscOptions,
    GhcPlatformOptions,
    GhcQuestionableOptions,
    GhcSimplifierOptions,
    GhcSpecialiserOptions,
    GhcStgLiftLamOptions,
]

_option_groups_optuna_samplers = [
    GhcInlinerOptionsOptunaSampler,
    GhcMiscOptionsOptunaSampler,
    GhcPlatformOptionsOptunaSampler,
    GhcQuestionableOptionsOptunaSampler,
    GhcSimplifierOptionsOptunaSampler,
    GhcSpecialiserOptionsOptunaSampler,
    GhcStgLiftLamOptionsOptunaSampler,
]


def add_all_ghc_option_groups_to_parser(arg_parser: ArgumentParser) -> None:
    """Get all GHC options as groups"""

    for option_group in _option_groups:
        option_group.add_group_to_parser(arg_parser)


def get_all_ghc_options() -> Mapping[str, GhcOption[Any]]:
    """Get all GHC options as a mapping from their names to their instances."""
    return {
        name: getattr(option_group, name) for option_group in _option_groups for name in option_group.__class_vars__
    }


def get_all_ghc_options_optuna_samplers() -> Mapping[str, SampleFn[Trial, Any]]:
    """Get all GHC options as a mapping from their names to their instances."""
    return {
        name: getattr(option_group, name)
        for option_group in _option_groups_optuna_samplers
        for name in option_group.__class_vars__
    }
