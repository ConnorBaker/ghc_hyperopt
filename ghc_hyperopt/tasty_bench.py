import csv
from collections.abc import Mapping, Sequence
from dataclasses import dataclass
from pathlib import Path
from typing import Self

from ghc_hyperopt.process_info import ProcessError, ProcessInfo
from ghc_hyperopt.rts_config import RtsConfig
from ghc_hyperopt.tasty_config import TastyConfig
from ghc_hyperopt.utils import get_logger

logger = get_logger(__name__)


class TastyBenchError(ProcessError):
    """An error occurred during the benchmark."""


@dataclass(frozen=True, unsafe_hash=True, slots=True, kw_only=True)
class TastyBenchmark:
    """Information about a single benchmark."""

    name: str
    """The name of the benchmark."""

    time_mean: float
    """The mean time taken to run, in picoseconds."""

    time_stdev: float
    """Twice the standard deviation of the time taken to run, in picoseconds."""

    mem_allocated: int
    """The amount of memory allocated, in bytes."""

    mem_copied: int
    """The amount of memory copied, in bytes."""

    mem_peak: int
    """The peak memory usage, in bytes."""

    @classmethod
    def from_csv(cls: type[Self], row: Mapping[str, str]) -> Self:
        """Parse a benchmark from a line of CSV read by DictReader."""
        # Name,Mean (ps),2*Stdev (ps),Allocated,Copied,Peak Memory
        return cls(
            name=row["Name"],
            time_mean=float(row["Mean (ps)"]),
            time_stdev=float(row["2*Stdev (ps)"]),
            mem_allocated=int(row["Allocated"]),
            mem_copied=int(row["Copied"]),
            mem_peak=int(row["Peak Memory"]),
        )


@dataclass(frozen=True, unsafe_hash=True, slots=True, kw_only=True)
class TastyBench:
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
    ) -> Self:
        try:
            process_info = ProcessInfo.do(
                args=[
                    executable_path.as_posix(),
                    "--quiet",
                    "--csv=/dev/stdout",
                    *tasty_config.to_flags(),
                    *rts_config.to_flags(),
                ],
                project_path=executable_path.parent,
                logger=logger,
            )
        except ProcessError as e:
            raise TastyBenchError(*e.args)

        # Read the results
        benchmarks: Sequence[TastyBenchmark] = sorted(
            map(TastyBenchmark.from_csv, csv.DictReader(process_info.stdout.splitlines())),
            key=lambda b: b.name,
        )

        # Return the build info
        return cls(
            executable_path=executable_path,
            tasty_config=tasty_config,
            rts_config=rts_config,
            benchmarks=benchmarks,
            process_info=process_info,
        )
