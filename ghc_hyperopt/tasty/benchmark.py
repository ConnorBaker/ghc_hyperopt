from collections.abc import Mapping, Sequence
from typing import Self, final

from optuna.study import StudyDirection

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


@final
class TastyBenchmarkOptimizationChoices(OurBaseModel):
    """The choices for the values to optimize."""

    time_mean: bool
    """Whether to minimize the mean time taken to run."""

    mem_allocated: bool
    """Whether to minimize the allocated memory."""

    mem_copied: bool
    """Whether to minimize the copied memory."""

    # TODO: We do this manually to ensure consistent ordering, even across runs. Is there a better way?

    def get_optimization_directions(self) -> Sequence[StudyDirection]:
        directions = []
        if self.time_mean:
            directions.append(StudyDirection.MINIMIZE)
        if self.mem_allocated:
            directions.append(StudyDirection.MINIMIZE)
        if self.mem_copied:
            directions.append(StudyDirection.MINIMIZE)
        return directions

    def gather_benchmark_results(self, benchmark: TastyBenchmark) -> Sequence[int]:
        results = []
        if self.time_mean:
            results.append(benchmark.time.mean)
        if self.mem_allocated:
            results.append(benchmark.mem.allocated)
        if self.mem_copied:
            results.append(benchmark.mem.copied)
        return results
