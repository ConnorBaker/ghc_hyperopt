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
class TastyBenchmarkMemoryInfo(OurBaseModel, Generic[A]):
    """Information from the RTS about the memory usage of a single benchmark."""

    allocated: A
    """The amount of memory allocated, in bytes."""

    copied: A
    """The amount of memory copied, in bytes."""

    peak: A
    """The peak memory usage, in bytes."""

    def fmap(self, f: Callable[[A], B]) -> TastyBenchmarkMemoryInfo[B]:
        return TastyBenchmarkMemoryInfo[B].model_validate(
            {k: f(v) for k, v in self},
            strict=True,
        )

    def zip(self, other: TastyBenchmarkMemoryInfo[B]) -> TastyBenchmarkMemoryInfo[tuple[A, B]]:
        return TastyBenchmarkMemoryInfo[tuple[A, B]].model_validate(
            {k: (v, getattr(other, k)) for k, v in self},
            strict=True,
        )

    def zip_with(
        self,
        other: TastyBenchmarkMemoryInfo[B],
        f: Callable[[A, B], C],
    ) -> TastyBenchmarkMemoryInfo[C]:
        return self.zip(other).fmap(lambda t: f(*t))
