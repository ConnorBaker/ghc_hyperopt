module Data.GHC.HyperOpt.Options.RTS where

import Data.Bool (Bool (True))
import Data.GHC.HyperOpt.Flags
  ( Flag (..),
    FlagKind (MkFlagKind),
    FlagPresent (..),
    FlagWithValue (..),
    FlagWithValueKind (..),
    Size (KiloBytes),
  )
import Data.GHC.HyperOpt.Options
  ( Option (..),
    ReifiableOption (MkReifiableOption),
    reifyOptions,
  )
import Data.String (IsString (fromString))
import Data.Text (Text)
import GHC.Num (fromInteger)

-- -- | Selects the non-moving mark-and-sweep garbage collector to manage the oldest generation (default: False)
nonmovingGC :: Option ('MkFlagKind 'WithoutValue Bool)
nonmovingGC =
  MkOption
    { flagPrefix = "--",
      flagName = "nonmoving-gc",
      flag = FlagSettable
    }

-- | Selects the copying garbage collector to manage all generations (default: False)
copyingGC :: Option ('MkFlagKind 'WithoutValue Bool)
copyingGC =
  MkOption
    { flagPrefix = "--",
      flagName = "copying-gc",
      flag = FlagSettable
    }

-- | Use mark-region for the oldest generation (experimental) (default: False)
markRegionForOldestGeneration :: Option ('MkFlagKind 'WithoutValue Bool)
markRegionForOldestGeneration =
  MkOption
    { flagPrefix = "-",
      flagName = "w",
      flag = FlagNegatable
    }

-- | Sets the initial thread stack size (default 1k)  e.g.: -ki4k -ki2m
initialThreadStackSize :: Option ('MkFlagKind ('WithValue 'ImmediatelyAfterFlag) Size)
initialThreadStackSize =
  MkOption
    { flagPrefix = "-",
      flagName = "ki",
      flag = FlagSize ImmediatelyAfterFlagKind
    }

-- | Sets the stack chunk size (default 32k)
stackChunkSize :: Option ('MkFlagKind ('WithValue 'ImmediatelyAfterFlag) Size)
stackChunkSize =
  MkOption
    { flagPrefix = "-",
      flagName = "kc",
      flag = FlagSize ImmediatelyAfterFlagKind
    }

-- | Sets the stack chunk buffer size (default 1k)
stackChunkBufferSize :: Option ('MkFlagKind ('WithValue 'ImmediatelyAfterFlag) Size)
stackChunkBufferSize =
  MkOption
    { flagPrefix = "-",
      flagName = "kb",
      flag = FlagSize ImmediatelyAfterFlagKind
    }

-- | Sets the minimum allocation area size (default 4m) e.g.: -A20m -A10k
minimumAllocationAreaSize :: Option ('MkFlagKind ('WithValue 'ImmediatelyAfterFlag) Size)
minimumAllocationAreaSize =
  MkOption
    { flagPrefix = "-",
      flagName = "A",
      flag = FlagSize ImmediatelyAfterFlagKind
    }

collectGcStatistics :: Option ('MkFlagKind 'WithoutValue Bool)
collectGcStatistics =
  MkOption
    { flagPrefix = "-",
      flagName = "T",
      flag = FlagSettable
    }

beginRtsOptions :: Option ('MkFlagKind 'WithoutValue Bool)
beginRtsOptions =
  MkOption
    { flagPrefix = "+",
      flagName = "RTS",
      flag = FlagSettable
    }

endRtsOptions :: Option ('MkFlagKind 'WithoutValue Bool)
endRtsOptions =
  MkOption
    { flagPrefix = "-",
      flagName = "RTS",
      flag = FlagSettable
    }

sampleReifiedOptions :: [Text]
sampleReifiedOptions =
  reifyOptions
    [ (MkReifiableOption nonmovingGC True),
      (MkReifiableOption initialThreadStackSize (KiloBytes 100))
    ]

-- >>> sampleReifiedOptions
-- ["--nonmoving-gc","-ki100k"]

-- TODO: Constraint: nonmovingGC and copyingGC are mutually exclusive

-- -- The following run time system options may be available (note that some
-- -- of these may not be usable unless this program was linked with the -rtsopts
-- -- flag):
-- --
-- --   -?        Prints this message and exits; the program is not executed
-- --   --info    Print information about the RTS used by this program
-- --
-- --   --nonmoving-gc
-- --             Selects the non-moving mark-and-sweep garbage collector to
-- --             manage the oldest generation.
-- --   --copying-gc
-- --             Selects the copying garbage collector to manage all generations.
-- --
-- --   -K<size>  Sets the maximum stack size (default: 80% of the heap)
-- --             e.g.: -K32k -K512k -K8M
-- --   -ki<size> Sets the initial thread stack size (default 1k)  e.g.: -ki4k -ki2m
-- --   -kc<size> Sets the stack chunk size (default 32k)
-- --   -kb<size> Sets the stack chunk buffer size (default 1k)
-- --
-- --   -A<size>  Sets the minimum allocation area size (default 4m) e.g.: -A20m -A10k
-- --   -AL<size> Sets the amount of large-object memory that can be allocated
-- --             before a GC is triggered (default: the value of -A)
-- --   -F<n>     Sets the collecting threshold for old generations as a factor of
-- --             the live data in that generation the last time it was collected
-- --             (default: 2.0)
-- --   -Fd<n>    Sets the inverse rate which memory is returned to the OS after being
-- --             optimistically retained after being allocated. Subsequent major
-- --             collections not caused by heap overflow will return an amount of
-- --             memory controlled by this factor (higher is slower). Setting the factor
-- --             to 0 means memory is not returned.
-- --             (default 4.0)
-- --   -n<size>  Allocation area chunk size (0 = disabled, default: 0)
-- --   -O<size>  Sets the minimum size of the old generation (default 1M)
-- --   -M<size>  Sets the maximum heap size (default unlimited)  e.g.: -M256k -M1G
-- --   -H<size>  Sets the minimum heap size (default 0M)   e.g.: -H24m  -H1G
-- --   -xb<addr> Sets the address from which a suitable start for the heap memory
-- --             will be searched from. This is useful if the default address
-- --             clashes with some third-party library.
-- --   -xn       Use the non-moving collector for the old generation.
-- --   -m<n>     Minimum % of heap which must be available (default 3%)
-- --   -G<n>     Number of generations (default: 2)
-- --   -c<n>     Use in-place compaction instead of copying in the oldest generation
-- --             when live data is at least <n>% of the maximum heap size set with
-- --             -M (default: 30%)
-- --   -c        Use in-place compaction for all oldest generation collections
-- --             (the default is to use copying)
-- --   -w        Use mark-region for the oldest generation (experimental)
-- --
-- --   -T         Collect GC statistics (useful for in-program statistics access)
-- --   -t[<file>] One-line GC statistics (if <file> omitted, uses stderr)
-- --   -s[<file>] Summary  GC statistics (if <file> omitted, uses stderr)
-- --   -S[<file>] Detailed GC statistics (if <file> omitted, uses stderr)
-- --
-- --
-- --   -Z         Don't squeeze out update frames on context switch
-- --   -B         Sound the bell at the start of each garbage collection
-- --   -h       Heap residency profile (output file <program>.hp)
-- --   -hT      Produce a heap profile grouped by closure type
-- --   -hi      Produce a heap profile grouped by info table address
-- --   -po<file>  Override profiling output file name prefix (program name by default)
-- --   -i<sec>  Time between heap profile samples (seconds, default: 0.1)
-- --   --no-automatic-heap-samples
-- --            Do not start the heap profile interval timer on start-up,
-- --            Rather, the application will be responsible for triggering
-- --            heap profiler samples.
-- --   -ol<file>  Send binary eventlog to <file> (default: <program>.eventlog)
-- --   -l[flags]  Log events to a file
-- --              where [flags] can contain:
-- --                 s    scheduler events
-- --                 g    GC and heap events
-- --                 n    non-moving GC heap census events
-- --                 p    par spark events (sampled)
-- --                 f    par spark events (full detail)
-- --                 u    user events (emitted from Haskell code)
-- --                 a    all event classes above
-- --                -x    disable an event class, for any flag above
-- --              the initial enabled event classes are 'sgpu'
-- --  --eventlog-flush-interval=<secs>
-- --              Periodically flush the eventlog at the specified interval.
-- --
-- --   -C<secs>  Context-switch interval in seconds.
-- --             0 or no argument means switch as often as possible.
-- --             Default: 0.02 sec.
-- --   -V<secs>  Master tick interval in seconds (0 == disable timer).
-- --             This sets the resolution for -C and the heap profile timer -i,
-- --             and is the frequency of time profile samples.
-- --             Default: 0.01 sec.
-- --
-- --   --install-signal-handlers=<yes|no>
-- --              Install signal handlers (default: yes)
-- --   --io-manager=<native|posix>
-- --              The I/O manager subsystem to use. (default: posix)
-- --   -xq        The allocation limit given to a thread after it receives
-- --              an AllocationLimitExceeded exception. (default: 100k)
-- --
-- --   -Mgrace=<n>
-- --              The amount of allocation after the program receives a
-- --              HeapOverflow exception before the exception is thrown again, if
-- --              the program is still exceeding the heap limit.
-- --
-- -- RTS options may also be specified using the GHCRTS environment variable.
-- --
-- -- Other RTS options may be available for programs compiled a different way.
-- -- The GHC User's Guide has full details.
