from collections.abc import Mapping, Sequence
from dataclasses import dataclass
from functools import partial
from typing import Any, Literal

from optuna import Trial

from ghc_hyperopt.ghc.option import GhcOption, GhcOptionGroup
from ghc_hyperopt.ghc.option.inliner import GhcInlinerOptions
from ghc_hyperopt.ghc.option.misc import GhcMiscOptions
from ghc_hyperopt.ghc.option.platform import GhcPlatformOptions
from ghc_hyperopt.ghc.option.questionable import GhcQuestionableOptions
from ghc_hyperopt.ghc.option.simplifier import GhcSimplifierOptions
from ghc_hyperopt.ghc.option.specialiser import GhcSpecialiserOptions
from ghc_hyperopt.ghc.option.stg_lift_lams import GhcStgLiftLamOptions
from ghc_hyperopt.utils import get_logger

logger = get_logger(__name__)


def get_all_ghc_option_groups() -> Sequence[GhcOptionGroup]:
    """Get all GHC options as groups"""
    return [
        GhcInlinerOptions,
        GhcMiscOptions,
        GhcPlatformOptions,
        GhcQuestionableOptions,
        GhcSimplifierOptions,
        GhcSpecialiserOptions,
        GhcStgLiftLamOptions,
    ]


def get_all_ghc_options() -> Mapping[str, GhcOption[Any]]:
    """Get all GHC options as a mapping from their names to their instances."""
    return {
        name: getattr(option_group, name)
        for option_group in get_all_ghc_option_groups()
        for name in option_group.__slots__
    }


class GhcConfigError(RuntimeError):
    """An error caused by an invalid GHC configuration."""


@dataclass(frozen=True, unsafe_hash=True, slots=True, kw_only=True)
class GhcConfig:
    """
    A GHC configuration instance.

    See https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html for more information.
    """

    optimization: Literal[0, 1, 2] = 2
    """The optimization level."""

    rtsopts: Literal["all"] = "all"
    """The RTS options."""

    # TODO: Need to add this to ghc.option and add the applicative do language extension
    optimal_applicative_do: bool = False
    """Optimal applicative do."""

    ghc_fixed_options: Mapping[str, GhcOption[Any]]
    """The GHC options which are unchanging."""

    ghc_tuneable_options: Mapping[str, GhcOption[Any]]
    """The GHC options which are tunable."""

    def __post_init__(self) -> None:
        """Validate the configuration, raising an exception if it is invalid."""

        # The fixed and tuneable options must be disjoint
        mixed_options = self.ghc_fixed_options.keys() & self.ghc_tuneable_options.keys()
        if mixed_options:
            raise ValueError(f"Options {mixed_options} are both fixed and tuneable.")

    def get_flags(self, trial: Trial) -> Sequence[str]:
        """
        Run one trial of the tuner.
        """
        fixed_flags = list(
            filter(
                None,
                map(
                    partial(GhcOption.to_flag, use="default"),
                    self.ghc_fixed_options.values(),
                ),
            )
        )
        logger.info("Fixed flags: %s", " ".join(fixed_flags))
        tuneable_flags = list(
            filter(
                None,
                map(
                    partial(GhcOption.to_flag, use="trial", trial=trial),
                    self.ghc_tuneable_options.values(),
                ),
            )
        )
        logger.info("Tuneable flags: %s", " ".join(tuneable_flags))
        ghc_args = [
            "-O" + str(self.optimization),
            "-rtsopts=" + self.rtsopts,
            *fixed_flags,
            *tuneable_flags,
        ]
        return ghc_args
