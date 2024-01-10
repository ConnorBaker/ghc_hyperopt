from typing import Self, TypeVar, final

from ghc_hyperopt.utils import OurBaseModel, percent_improvement

A = TypeVar("A")
B = TypeVar("B")
C = TypeVar("C")


@final
class TastyBenchmarkResult(OurBaseModel):
    """The result of a single benchmark."""

    raw: int
    """The raw result."""

    percent_improvement: float
    """The percent improvement."""

    @classmethod
    def from_raw(cls, raw: int, baseline: int) -> Self:
        return cls(
            raw=raw,
            percent_improvement=percent_improvement(raw, baseline),
        )
