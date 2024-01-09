from typing import TypeVar, final

from ghc_hyperopt.utils import OurBaseModel

A = TypeVar("A")
B = TypeVar("B")
C = TypeVar("C")


# NOTE: Issues with recursive generics:
# https://github.com/pydantic/pydantic/issues/7096#issuecomment-1678031081


@final
class TastyBenchmarkResult(OurBaseModel):
    """The result of a single benchmark."""

    raw: int
    """The raw result."""

    percent_improvement: float
    """The percent improvement."""
