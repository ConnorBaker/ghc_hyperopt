from __future__ import annotations

from collections.abc import Callable
from typing import Generic, TypeVar, final

from ghc_hyperopt.utils import OurBaseModel

A = TypeVar("A")
B = TypeVar("B")
C = TypeVar("C")


# NOTE: Issues with recursive generics:
# https://github.com/pydantic/pydantic/issues/7096#issuecomment-1678031081


@final
class TastyBenchmarkTimeInfo(OurBaseModel, Generic[A]):
    """Information from the RTS about the time taken to run a single benchmark."""

    mean: A
    """The mean time taken to run, in picoseconds."""

    stdev: A
    """Twice the standard deviation of the time taken to run, in picoseconds."""

    def fmap(self, f: Callable[[A], B]) -> TastyBenchmarkTimeInfo[B]:
        return TastyBenchmarkTimeInfo[B].model_validate(
            {k: f(v) for k, v in self},
            strict=True,
        )

    def zip(self, other: TastyBenchmarkTimeInfo[B]) -> TastyBenchmarkTimeInfo[tuple[A, B]]:
        return TastyBenchmarkTimeInfo[tuple[A, B]].model_validate(
            {k: (v, getattr(other, k)) for k, v in self},
            strict=True,
        )

    def zip_with(
        self,
        other: TastyBenchmarkTimeInfo[B],
        f: Callable[[A, B], C],
    ) -> TastyBenchmarkTimeInfo[C]:
        return self.zip(other).fmap(lambda t: f(*t))
