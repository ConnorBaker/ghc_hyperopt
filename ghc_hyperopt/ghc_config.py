from collections.abc import Sequence
from dataclasses import dataclass
from functools import partial
from operator import ne
from typing import Annotated, Literal, Self

from optuna import Trial

from ghc_hyperopt.utils import suggest_bool, suggest_int


class GHCConfigError(RuntimeError):
    """An error caused by an invalid GHC configuration."""


@dataclass(frozen=True, unsafe_hash=True, slots=True, kw_only=True)
class GHCConfig:
    """
    A GHC configuration instance.

    See https://downloads.haskell.org/ghc/latest/docs/users_guide/using-optimisation.html for more information.
    """

    optimization: Annotated[Literal[0, 1, 2], suggest_int(0, 2)] = 0
    """The optimization level."""

    eager_blackholing: Annotated[bool, suggest_bool()] = False
    """Eager blackholing."""

    specialise_aggressively: Annotated[bool, suggest_bool()] = False
    """Specialize aggressively."""

    expose_all_unfoldings: Annotated[bool, suggest_bool()] = False
    """Expose all unfoldings."""

    late_dmd_anal: Annotated[bool, suggest_bool()] = False
    """Late demand analysis."""

    max_simplifier_iterations: Annotated[int, suggest_int(4, 2**10, log=True)] = 4
    """Maximum number of simplifier iterations."""

    simplifier_phases: Annotated[int, suggest_int(1, 2**8, log=True)] = 1
    """
    Maximum number of simplifier phases.

    NOTE: This value is ignored if optimization is set to 0.
    """

    simpl_tick_factor: Annotated[int, suggest_int(100, 2**14, log=True)] = 100
    """Maximum number of simplifier ticks."""

    late_specialise: Annotated[bool, suggest_bool()] = False
    """Late specialization."""

    static_argument_transformation: Annotated[bool, suggest_bool()] = False
    """Static argument transformation."""

    optimal_applicative_do: Annotated[bool, suggest_bool()] = False
    """Optimal applicative do."""

    def __post_init__(self) -> None:
        """Validate the configuration, raising an exception if it is invalid."""
        pass

    def to_flags(self) -> Sequence[str]:
        # Special case for optimization, which doesn't follow naming convention.
        options: list[str] = list(filter(partial(ne, "optimization"), self.__slots__))
        # Always enable RTS options.
        flags: list[str] = ["-rtsopts=all", f"-O{self.optimization}"]
        for option in options:
            ghc_option_name = option.replace("_", "-")
            match getattr(self, option):
                case bool(b):
                    flags.append(f"-f{ghc_option_name}" if b else f"-fno-{ghc_option_name}")
                case int(n):
                    flags.append(f"-f{ghc_option_name}={n}")
                case _:
                    raise NotImplementedError(f"Option {option} is not implemented.")

        return flags

    @classmethod
    def from_trial(cls: type[Self], trial: Trial) -> Self:
        return cls(**{name: cls.__annotations__[name].__metadata__[0](name, trial) for name in cls.__slots__})
