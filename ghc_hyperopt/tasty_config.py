from collections.abc import Sequence
from dataclasses import dataclass
from typing import Literal


# TODO: Add support for options read from JSON or something similar because users can implement
# their own options with Tasty.
@dataclass(frozen=True, unsafe_hash=True, slots=True, kw_only=True)
class TastyConfig:
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

    list_tests: bool = False
    """Do not run the tests; just print their names."""

    num_threads: None | int = None
    """Number of threads to use for tests execution (default: # of cores/capabilities)."""

    csv: None | str = None
    """File to write results in CSV format."""

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
        options: list[str] = list(self.__slots__)
        flags: list[str] = []
        for option in options:
            tasty_option_name = "--" + option.replace("_", "-")
            match getattr(self, option):
                case None | False:
                    continue
                case True:
                    flags.append(tasty_option_name)
                case int() | str() as v:
                    flags.append(tasty_option_name + f"={v}")
                case _:
                    raise NotImplementedError(f"Option {option} is not implemented.")

        return flags
