from collections.abc import Mapping

from ghc_hyperopt.config import StudyDirection
from ghc_hyperopt.tasty.benchmark import (
    TastyBenchmark,
    TastyBenchmarkMemoryInfo,
    TastyBenchmarkTimeInfo,
)

# NOTE: We are MAXIMIZING the percent improvement -- that's our objective function -- except for
# the stdev of the time, which we are MINIMIZING.
OPTIMIZATION_DIRECTIONS: TastyBenchmark[StudyDirection] = TastyBenchmark[StudyDirection](
    name="OPTIMIZATION_DIRECTIONS",
    time=TastyBenchmarkTimeInfo[StudyDirection](
        mean="maximize",
        stdev="minimize",
    ),
    mem=TastyBenchmarkMemoryInfo[StudyDirection](
        allocated="maximize",
        copied="maximize",
        peak="maximize",
    ),
)

OPTIMIZATION_CHOICES: TastyBenchmark[bool] = TastyBenchmark[bool](
    name="OPTIMIZATION_CHOICES",
    time=TastyBenchmarkTimeInfo[bool](
        mean=True,
        stdev=False,
    ),
    mem=TastyBenchmarkMemoryInfo[bool](
        allocated=False,
        copied=False,
        peak=False,
    ),
)
assert sum(OPTIMIZATION_CHOICES.to_list()) == 1, "Exactly one optimization direction must be enabled"

# Maps benchmark names to weights for their contribution to the overall objective function
# If a name is not present, it is assumed to have a weight of 1.0
OPTIMIZATION_WEIGHTS: Mapping[str, TastyBenchmark[float]] = {}
