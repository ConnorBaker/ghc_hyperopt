from collections.abc import Sequence
from typing import Literal, final

from ghc_hyperopt.utils import OurBaseModel


@final
class TastyConfig(OurBaseModel):
    """
    Arguments to be provided to the tasty-benchmarking executable.

    If the argument is None or False, it will not be passed to the executable.

    If the argument is True, it will be passed as a flag to the executable.

    If the argument is an integer or a string, it will be passed as a flag to the executable.
    """

    pattern: None | str = None
    """Select only tests which satisfy a pattern or awk expression."""

    timeout: None | str = None
    """Timeout for individual tests (suffixes: ms,s,m,h; default: s)."""

    num_threads: Literal[1] = 1
    """Number of threads to use for tests execution (default: # of cores/capabilities)."""

    stdev: None | str = None
    """
    Target relative standard deviation of measurements in percents (5 by default). Large values correspond to fast and
    loose benchmarks, and small ones to long and precise. If it takes far too long, consider setting --timeout, which
    will interrupt benchmarks, potentially before reaching the target deviation.
    """

    time_mode: Literal["cpu", "wall"] = "cpu"
    """Whether to measure CPU time ("cpu") or wall-clock time ("wall") (default: cpu)."""

    def to_flags(self) -> Sequence[str]:
        """Convert the configuration to a list of flags."""
        flags: list[str] = []
        # NOTE: Pydantic lets us iterate over the fields of the model
        for key, value in self:
            tasty_option_name = "--" + key.replace("_", "-")
            match value:
                case None | False:
                    continue
                case True:
                    flags.append(tasty_option_name)
                case int() | str():
                    flags.append(tasty_option_name + f"={value}")
                case _:
                    raise NotImplementedError(f"Option {key} is not implemented.")

        return flags
