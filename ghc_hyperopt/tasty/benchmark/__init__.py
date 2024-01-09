from __future__ import annotations

import math
from collections.abc import Callable, Mapping, Sequence
from typing import Generic, TypeVar, final

from ghc_hyperopt.tasty.benchmark.memory_info import TastyBenchmarkMemoryInfo
from ghc_hyperopt.tasty.benchmark.result import TastyBenchmarkResult
from ghc_hyperopt.tasty.benchmark.time_info import TastyBenchmarkTimeInfo
from ghc_hyperopt.tasty.error import TastyBenchmarkParseError
from ghc_hyperopt.utils import OurBaseModel

A = TypeVar("A")
B = TypeVar("B")
C = TypeVar("C")


# NOTE: Issues with recursive generics:
# https://github.com/pydantic/pydantic/issues/7096#issuecomment-1678031081


@final
class TastyBenchmark(OurBaseModel, Generic[A]):
    """Information about a single benchmark."""

    name: str
    """The name of the benchmark."""

    time: TastyBenchmarkTimeInfo[A]
    """Information about the time taken to run the benchmark."""

    mem: TastyBenchmarkMemoryInfo[A]
    """Information about the memory usage of the benchmark."""

    def fmap(self, f: Callable[[A], B]) -> TastyBenchmark[B]:
        return TastyBenchmark[B].model_validate(
            {k: v if k == "name" else v.fmap(f) for k, v in self},
            strict=True,
        )

    def zip(self, other: TastyBenchmark[B]) -> TastyBenchmark[tuple[A, B]]:
        return TastyBenchmark[tuple[A, B]].model_validate(
            {k: v if k == "name" else v.zip(getattr(other, k)) for k, v in self},
            strict=True,
        )

    def zip_with(
        self,
        other: TastyBenchmark[B],
        f: Callable[[A, B], C],
    ) -> TastyBenchmark[C]:
        return self.zip(other).fmap(lambda t: f(*t))

    @staticmethod
    def percent_improvement(new: float, old: float) -> float:
        denom = math.copysign(1e-6, old) if math.isclose(old, 0) else old
        return (new - old) / denom * 100

    @staticmethod
    def from_csv(row: Mapping[str, str]) -> TastyBenchmarkParseError | TastyBenchmark[int]:
        """Parse a benchmark from a line of CSV read by DictReader."""
        # Name,Mean (ps),2*Stdev (ps),Allocated,Copied,Peak Memory
        try:
            return TastyBenchmark[str](
                name=row["Name"],
                time=TastyBenchmarkTimeInfo[str](
                    mean=row["Mean (ps)"],
                    stdev=row["2*Stdev (ps)"],
                ),
                mem=TastyBenchmarkMemoryInfo[str](
                    allocated=row["Allocated"],
                    copied=row["Copied"],
                    peak=row["Peak Memory"],
                ),
            ).fmap(int)
        except Exception as e:
            return TastyBenchmarkParseError(
                f"Could not create {TastyBenchmark[TastyBenchmarkResult].__name__} from {row}"
            ).with_traceback(e.__traceback__)

    @staticmethod
    def from_raw(
        benchmark: TastyBenchmark[int],
        baseline: TastyBenchmark[int],
    ) -> TastyBenchmark[TastyBenchmarkResult]:
        """Convert a benchmark from raw values to a percent improvement."""
        assert benchmark.name == baseline.name
        return benchmark.zip_with(
            baseline,
            lambda new, old: TastyBenchmarkResult(
                raw=new,
                percent_improvement=TastyBenchmark.percent_improvement(new, old),
            ),
        )

    def to_list(self) -> Sequence[A]:
        """Convert the benchmark to a list."""
        flattened = []
        for metric_type, metrics in self:
            if metric_type == "name":
                continue

            match metrics:
                case TastyBenchmarkTimeInfo() | TastyBenchmarkMemoryInfo():
                    for _metric_name, metric_value in metrics:
                        flattened.append(metric_value)

                case _:
                    raise NotImplementedError(f"Unknown metric type {metric_type}")

        return flattened
