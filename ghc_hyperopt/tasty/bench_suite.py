import csv
from collections.abc import Sequence
from pathlib import Path
from typing import Self, final

from optuna.study import StudyDirection

from ghc_hyperopt.process_info import ProcessError, ProcessInfo
from ghc_hyperopt.rts_config import RtsConfig
from ghc_hyperopt.tasty.benchmark import TastyBenchmark, TastyBenchmarkOptimizationChoices
from ghc_hyperopt.tasty.error import TastyBenchmarkParseError, TastyBenchSuiteRuntimeError
from ghc_hyperopt.tasty_config import TastyConfig
from ghc_hyperopt.utils import OurBaseModel


@final
class TastyBenchSuite(OurBaseModel):
    """Information about the benchmarks."""

    executable_path: Path
    """The path to the benchmark executable."""

    tasty_config: TastyConfig
    """The tasty configuration used for the benchmarks."""

    rts_config: RtsConfig
    """The RTS configuration used for the benchmarks."""

    benchmarks: Sequence[TastyBenchmark]
    """The benchmarks."""

    process_info: ProcessInfo
    """Information about the benchmarking process."""

    @classmethod
    def do(
        cls: type[Self],
        executable_path: Path,
        tasty_config: TastyConfig,
        rts_config: RtsConfig,
    ) -> TastyBenchSuiteRuntimeError | TastyBenchmarkParseError | Self:
        process_info = ProcessInfo.do(
            args=[
                executable_path.as_posix(),
                "--quiet",
                "--csv=/dev/stdout",
                *tasty_config.to_flags(),
                *rts_config.to_flags(),
            ],
            project_path=executable_path.parent,
        )

        if isinstance(process_info, ProcessError):
            return TastyBenchSuiteRuntimeError(process_info.msg)

        # Read the results
        benchmarks: list[TastyBenchmark] = []
        for row in csv.DictReader(process_info.stdout.splitlines()):
            benchmark = TastyBenchmark.from_csv(row)
            if isinstance(benchmark, TastyBenchmarkParseError):
                return benchmark
            else:
                benchmarks.append(benchmark)

        benchmarks.sort(key=lambda b: b.name)

        # Return the build info
        return cls(
            executable_path=executable_path,
            tasty_config=tasty_config,
            rts_config=rts_config,
            benchmarks=benchmarks,
            process_info=process_info,
        )

    def get_optimization_directions(
        self,
        optimization_choices: TastyBenchmarkOptimizationChoices,
    ) -> Sequence[StudyDirection]:
        return list(optimization_choices.get_optimization_directions()) * len(self.benchmarks)

    def gather_benchmark_results(
        self,
        optimization_choices: TastyBenchmarkOptimizationChoices,
    ) -> Sequence[int]:
        results = []
        for benchmark in self.benchmarks:
            results.extend(optimization_choices.gather_benchmark_results(benchmark))
        return results
