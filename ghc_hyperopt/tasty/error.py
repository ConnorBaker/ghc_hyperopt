from typing import TypeAlias, final

from ghc_hyperopt.process_info import ProcessError


@final
class TastyBenchmarkParseError(ProcessError):
    """An error occurred while parsing the results of a benchmark."""

    pass


@final
class TastyBenchSuiteRuntimeError(ProcessError):
    """An error occurred during the benchmark suite run."""

    pass


@final
class TastyBenchSuiteUnknownError(ProcessError):
    """An unknown error occurred during the benchmark."""

    pass


TastyBenchSuiteError: TypeAlias = TastyBenchmarkParseError | TastyBenchSuiteRuntimeError | TastyBenchSuiteUnknownError
