from collections.abc import Mapping
from typing import Self, final

from ghc_hyperopt.tasty.error import TastyBenchmarkParseError
from ghc_hyperopt.utils import OurBaseModel, get_logger

logger = get_logger(__name__)


@final
class TastyBenchmarkMemoryInfo(OurBaseModel):
    """Information from the RTS about the memory usage of a single benchmark."""

    allocated: int
    """The amount of memory allocated, in bytes."""

    copied: int
    """The amount of memory copied, in bytes."""

    peak: int
    """The peak memory usage, in bytes."""


@final
class TastyBenchmarkTimeInfo(OurBaseModel):
    """Information from the RTS about the time taken to run a single benchmark."""

    mean: int
    """The mean time taken to run, in picoseconds."""

    stdev: int
    """Twice the standard deviation of the time taken to run, in picoseconds."""


@final
class TastyBenchmark(OurBaseModel):
    """Information about a single benchmark."""

    name: str
    """The name of the benchmark."""

    time: TastyBenchmarkTimeInfo
    """Information about the time taken to run the benchmark."""

    mem: TastyBenchmarkMemoryInfo
    """Information about the memory usage of the benchmark."""

    @classmethod
    def from_csv(cls: type[Self], row: Mapping[str, str]) -> TastyBenchmarkParseError | Self:
        """Parse a benchmark from a line of CSV read by DictReader."""
        # Name,Mean (ps),2*Stdev (ps),Allocated,Copied,Peak Memory
        try:
            return cls(
                name=row["Name"],
                time=TastyBenchmarkTimeInfo(
                    mean=int(row["Mean (ps)"]),
                    stdev=int(row["2*Stdev (ps)"]),
                ),
                mem=TastyBenchmarkMemoryInfo(
                    allocated=int(row["Allocated"]),
                    copied=int(row["Copied"]),
                    peak=int(row["Peak Memory"]),
                ),
            )
        except Exception as e:
            return TastyBenchmarkParseError(f"Could not create {cls.__name__} from {row}").with_traceback(
                e.__traceback__
            )
