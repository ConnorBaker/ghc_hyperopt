from collections.abc import Mapping
from statistics import fmean
from typing import Literal

from ghc_hyperopt.tasty.bench_suite import TastyBenchSuite
from ghc_hyperopt.tasty.benchmark import (
    TastyBenchmark,
)
from ghc_hyperopt.utils import percent_improvement

type StudyDirection = Literal["maximize", "minimize"]


def to_study_direction(direction: str) -> StudyDirection:
    match direction:
        case "maximize" | "minimize":
            return direction
        case _:
            raise ValueError(f"Invalid study direction: {direction}")


def get_optimization_direction(
    optimization_choices: TastyBenchmark[bool],
    optimization_directions: TastyBenchmark[StudyDirection],
) -> StudyDirection:
    match [direction for direction, enabled in optimization_directions.zip(optimization_choices).to_list() if enabled]:
        case []:
            raise ValueError("Exactly one optimization direction must be enabled")
        case [direction]:
            return to_study_direction(direction)
        case _:
            raise ValueError("Exactly one optimization direction must be enabled")


def objective(
    bench_info: TastyBenchSuite,
    optimization_choices: TastyBenchmark[bool],
    optimization_weights: Mapping[str, TastyBenchmark[float]],
    baseline: TastyBenchSuite,
) -> float:
    """The objective function for the optimization."""

    all_per_inc_w_weights: list[tuple[float, float]] = []
    for benchmark, baseline_benchmark in zip(bench_info.benchmarks, baseline.benchmarks, strict=True):
        assert benchmark.name == baseline_benchmark.name
        all_per_inc_w_weights.extend(
            filter(
                # Remove percents and weights for metrics that are not enabled
                None,
                (
                    # Get the percent improvement for each metric relative to the baseline
                    benchmark.zip_with(baseline_benchmark, percent_improvement)
                    # Zip with the provided weights, or 1.0 if no weights are provided for the benchmark
                    .zip(optimization_weights.get(benchmark.name, TastyBenchmark[float].pure(1.0)))
                    # Set the value to None if the optimization choice is disabled
                    .zip_with(optimization_choices, lambda values, enabled: values if enabled else None)
                    # Convert to a list
                    .to_list()
                ),
            )
        )

    return fmean(*zip(*all_per_inc_w_weights))
