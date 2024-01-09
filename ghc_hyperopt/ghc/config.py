from collections.abc import Mapping, Sequence
from dataclasses import dataclass
from typing import Any, Literal, Self, final

from optuna.trial import Trial
from pydantic import model_validator

from ghc_hyperopt.ghc.option import GhcOption
from ghc_hyperopt.utils import OurBaseException, SampleFn, get_logger

logger = get_logger(__name__)


class GhcConfigError(OurBaseException):
    """An error caused by an invalid GHC configuration."""


# TODO: Not generic over the choice of sampler
# TODO: Won't work as a Pydantic model because type check fails for the ghc_tuneable_options
@final
@dataclass(frozen=True)
class GhcConfig:
    """
    A GHC configuration instance.

    See https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html for more information.
    """

    ghc_fixed_options: Mapping[str, GhcOption[Any]]
    """The GHC options which are unchanging."""

    ghc_tuneable_options: Mapping[str, tuple[GhcOption[Any], SampleFn[Trial, Any]]]
    """The GHC options which are tunable."""

    optimization: Literal[0, 1, 2] = 2
    """The optimization level."""

    rtsopts: Literal["all"] = "all"
    """The RTS options."""

    # TODO: Need to add this to ghc.option and add the applicative do language extension
    optimal_applicative_do: bool = False
    """Optimal applicative do."""

    @model_validator(mode="after")
    def no_fixed_and_tuneable_overlap(self: Self) -> Self:
        """Validate the configuration, raising an exception if it is invalid."""

        # The fixed and tuneable options must be disjoint
        mixed_options = self.ghc_fixed_options.keys() & self.ghc_tuneable_options.keys()
        if mixed_options:
            raise ValueError(f"Options {mixed_options} are both fixed and tuneable.")

        return self

    def get_fixed_flags(self) -> Sequence[str]:
        """
        Get the fixed flags.
        """
        return [
            flag
            for option in self.ghc_fixed_options.values()
            for flag in [GhcOption.to_flag(option, use="default")]
            if flag is not None
        ]

    def get_tuneable_flags(self, sampler: Trial) -> Sequence[str]:
        """
        Get the tuneable flags.
        """
        return [
            flag
            for name, (option, sample_fn) in self.ghc_tuneable_options.items()
            for flag in [GhcOption.to_flag(option, use="value", value=sample_fn(name, sampler))]
            if flag is not None
        ]

    def get_flags(self, sampler: Trial) -> Sequence[str]:
        """
        Run one trial of the tuner.
        """
        fixed_flags = self.get_fixed_flags()
        logger.info("Fixed flags: %s", " ".join(fixed_flags))
        tuneable_flags = self.get_tuneable_flags(sampler)
        logger.info("Tuneable flags: %s", " ".join(tuneable_flags))
        ghc_args = [
            "-O" + str(self.optimization),
            "-rtsopts=" + self.rtsopts,
            *fixed_flags,
            *tuneable_flags,
        ]
        return ghc_args
