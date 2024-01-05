from collections.abc import Sequence
from dataclasses import dataclass
from typing import Annotated, Self

from optuna import Trial

from ghc_hyperopt.utils import suggest_int

# Generally, kilobyte is our base unit for RTS options involving sizes.
KILOBYTE: int = 1024
# A megabyte is 1024 kilobytes.
MEGABYTE: int = 1024 * KILOBYTE
# A gigabyte is 1024 megabytes.
GIGABYTE: int = 1024 * MEGABYTE


class RTSConfigError(RuntimeError):
    """An error caused by an invalid RTS configuration."""

    broken_invariant: str
    """The invariant that was broken."""

    def __init__(self, broken_invariant: str) -> None:
        """Initialize the error."""
        self.broken_invariant = broken_invariant
        super().__init__(self.broken_invariant)


@dataclass(frozen=True, unsafe_hash=True, slots=True, kw_only=True)
class RTSConfig:
    """An RTS configuration instance."""

    # TODO: Good god the names of some of these configurations are ugly.

    ki: Annotated[int, suggest_int(1 * KILOBYTE, 512 * MEGABYTE, log=True)] = 1 * KILOBYTE
    """
    Sets the initial thread stack size (default 1k)  e.g.: -ki4k -ki2m

    NOTE: This value is always specified in BYTES.
    """

    kc: Annotated[int, suggest_int(32 * KILOBYTE, 64 * MEGABYTE, log=True)] = 32 * KILOBYTE
    """
    Sets the stack chunk size (default 32k)

    NOTE: This value is always specified in BYTES.
    """

    kb: Annotated[int, suggest_int(1 * KILOBYTE, 1 * MEGABYTE, log=True)] = 1 * KILOBYTE
    """
    Sets the stack chunk buffer size (default 1k)

    NOTE: This value is always specified in BYTES.
    """

    A: Annotated[int, suggest_int(4 * MEGABYTE, 512 * MEGABYTE, log=True)] = 4 * MEGABYTE
    """
    Sets the minimum allocation area size (default 4m) e.g.: -A20m -A10k

    NOTE: This value is always specified in BYTES.
    """

    def __post_init__(self) -> None:
        """Ensure that the RTS configuration is valid."""
        # stack chunk buffer size (-kb) must be less than 50% of the stack chunk size (-kc)
        if self.kb >= self.kc / 2:
            raise RTSConfigError(f"kb ({self.kb}) must be less than 50% of kc ({self.kc})")
        pass

    def to_flags(self) -> Sequence[str]:
        """Convert the configuration to a sequence of flags."""

        options: list[str] = list(self.__slots__)
        # Must always collect GC statistics (useful for in-program statistics access)
        flags: list[str] = ["+RTS", "-T"]
        for option in options:
            match getattr(self, option):
                case int(n):
                    flags.append(f"-{option}{n}")
                case _:
                    raise NotImplementedError(f"Option {option} is not implemented.")

        flags.append("-RTS")
        return flags

    @classmethod
    def from_trial(cls: type[Self], trial: Trial) -> Self:
        return cls(**{name: cls.__annotations__[name].__metadata__[0](name, trial) for name in cls.__slots__})


# bench-fib: The following run time system options may be available (note that some
# bench-fib: of these may not be usable unless this program was linked with the -rtsopts
# bench-fib: flag):
# bench-fib:
# bench-fib:   -?        Prints this message and exits; the program is not executed
# bench-fib:   --info    Print information about the RTS used by this program
# bench-fib:
# bench-fib:   --nonmoving-gc
# bench-fib:             Selects the non-moving mark-and-sweep garbage collector to
# bench-fib:             manage the oldest generation.
# bench-fib:   --copying-gc
# bench-fib:             Selects the copying garbage collector to manage all generations.
# bench-fib:
# bench-fib:   -K<size>  Sets the maximum stack size (default: 80% of the heap)
# bench-fib:             e.g.: -K32k -K512k -K8M
# bench-fib:   -ki<size> Sets the initial thread stack size (default 1k)  e.g.: -ki4k -ki2m
# bench-fib:   -kc<size> Sets the stack chunk size (default 32k)
# bench-fib:   -kb<size> Sets the stack chunk buffer size (default 1k)
# bench-fib:
# bench-fib:   -A<size>  Sets the minimum allocation area size (default 4m) e.g.: -A20m -A10k
# bench-fib:   -AL<size> Sets the amount of large-object memory that can be allocated
# bench-fib:             before a GC is triggered (default: the value of -A)
# bench-fib:   -F<n>     Sets the collecting threshold for old generations as a factor of
# bench-fib:             the live data in that generation the last time it was collected
# bench-fib:             (default: 2.0)
# bench-fib:   -Fd<n>    Sets the inverse rate which memory is returned to the OS after being
# bench-fib:             optimistically retained after being allocated. Subsequent major
# bench-fib:             collections not caused by heap overflow will return an amount of
# bench-fib:             memory controlled by this factor (higher is slower). Setting the factor
# bench-fib:             to 0 means memory is not returned.
# bench-fib:             (default 4.0)
# bench-fib:   -n<size>  Allocation area chunk size (0 = disabled, default: 0)
# bench-fib:   -O<size>  Sets the minimum size of the old generation (default 1M)
# bench-fib:   -M<size>  Sets the maximum heap size (default unlimited)  e.g.: -M256k -M1G
# bench-fib:   -H<size>  Sets the minimum heap size (default 0M)   e.g.: -H24m  -H1G
# bench-fib:   -xb<addr> Sets the address from which a suitable start for the heap memory
# bench-fib:             will be searched from. This is useful if the default address
# bench-fib:             clashes with some third-party library.
# bench-fib:   -xn       Use the non-moving collector for the old generation.
# bench-fib:   -m<n>     Minimum % of heap which must be available (default 3%)
# bench-fib:   -G<n>     Number of generations (default: 2)
# bench-fib:   -c<n>     Use in-place compaction instead of copying in the oldest generation
# bench-fib:             when live data is at least <n>% of the maximum heap size set with
# bench-fib:             -M (default: 30%)
# bench-fib:   -c        Use in-place compaction for all oldest generation collections
# bench-fib:             (the default is to use copying)
# bench-fib:   -w        Use mark-region for the oldest generation (experimental)
# bench-fib:
# bench-fib:   -T         Collect GC statistics (useful for in-program statistics access)
# bench-fib:   -t[<file>] One-line GC statistics (if <file> omitted, uses stderr)
# bench-fib:   -s[<file>] Summary  GC statistics (if <file> omitted, uses stderr)
# bench-fib:   -S[<file>] Detailed GC statistics (if <file> omitted, uses stderr)
# bench-fib:
# bench-fib:
# bench-fib:   -Z         Don't squeeze out update frames on context switch
# bench-fib:   -B         Sound the bell at the start of each garbage collection
# bench-fib:   -h       Heap residency profile (output file <program>.hp)
# bench-fib:   -hT      Produce a heap profile grouped by closure type
# bench-fib:   -hi      Produce a heap profile grouped by info table address
# bench-fib:   -po<file>  Override profiling output file name prefix (program name by default)
# bench-fib:   -i<sec>  Time between heap profile samples (seconds, default: 0.1)
# bench-fib:   --no-automatic-heap-samples
# bench-fib:            Do not start the heap profile interval timer on start-up,
# bench-fib:            Rather, the application will be responsible for triggering
# bench-fib:            heap profiler samples.
# bench-fib:   -ol<file>  Send binary eventlog to <file> (default: <program>.eventlog)
# bench-fib:   -l[flags]  Log events to a file
# bench-fib:              where [flags] can contain:
# bench-fib:                 s    scheduler events
# bench-fib:                 g    GC and heap events
# bench-fib:                 n    non-moving GC heap census events
# bench-fib:                 p    par spark events (sampled)
# bench-fib:                 f    par spark events (full detail)
# bench-fib:                 u    user events (emitted from Haskell code)
# bench-fib:                 a    all event classes above
# bench-fib:                -x    disable an event class, for any flag above
# bench-fib:              the initial enabled event classes are 'sgpu'
# bench-fib:  --eventlog-flush-interval=<secs>
# bench-fib:              Periodically flush the eventlog at the specified interval.
# bench-fib:
# bench-fib:   -C<secs>  Context-switch interval in seconds.
# bench-fib:             0 or no argument means switch as often as possible.
# bench-fib:             Default: 0.02 sec.
# bench-fib:   -V<secs>  Master tick interval in seconds (0 == disable timer).
# bench-fib:             This sets the resolution for -C and the heap profile timer -i,
# bench-fib:             and is the frequency of time profile samples.
# bench-fib:             Default: 0.01 sec.
# bench-fib:
# bench-fib:   --install-signal-handlers=<yes|no>
# bench-fib:              Install signal handlers (default: yes)
# bench-fib:   --io-manager=<native|posix>
# bench-fib:              The I/O manager subsystem to use. (default: posix)
# bench-fib:   -xq        The allocation limit given to a thread after it receives
# bench-fib:              an AllocationLimitExceeded exception. (default: 100k)
# bench-fib:
# bench-fib:   -Mgrace=<n>
# bench-fib:              The amount of allocation after the program receives a
# bench-fib:              HeapOverflow exception before the exception is thrown again, if
# bench-fib:              the program is still exceeding the heap limit.
# bench-fib:
# bench-fib: RTS options may also be specified using the GHCRTS environment variable.
# bench-fib:
# bench-fib: Other RTS options may be available for programs compiled a different way.
# bench-fib: The GHC User's Guide has full details.
