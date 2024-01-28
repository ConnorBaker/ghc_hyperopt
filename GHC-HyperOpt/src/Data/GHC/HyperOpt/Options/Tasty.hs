module Data.GHC.HyperOpt.Options.Tasty where

import Data.Bool (Bool (False, True))
import Data.GHC.HyperOpt.Flags
  ( Flag (..),
    FlagAppearance (..),
    FlagKind (MkFlagKind),
    Size (KiloBytes, MegaBytes),
  )
import Data.GHC.HyperOpt.Options
  ( Option (..),
    ReifiableOption (MkReifiableOption),
    reifyOptions,
  )
import Data.Int (Int)
import Data.String (IsString (fromString))
import Data.Text (Text)
import GHC.Num (fromInteger)
import System.IO (FilePath)

-- | Sets the number of threads to use (default: # of cores/capabilities)
numThreads :: Option ('MkFlagKind 'FlagAppearanceWithValueOrAbsent Int)
numThreads =
  MkOption
    { flagPrefix = "--",
      flagName = "num-threads",
      flag = FlagWithInt
    }

-- | Do not produce any output; indicate success only by the exit code (default: False)
quiet :: Option ('MkFlagKind 'FlagAppearanceWithoutValueOrAbsent Bool)
quiet =
  MkOption
    { flagPrefix = "--",
      flagName = "quiet",
      flag = FlagWithoutValueOrAbsent
    }

-- | File to write results in CSV format
csv :: Option ('MkFlagKind 'FlagAppearanceWithValueOrAbsent FilePath)
csv =
  MkOption
    { flagPrefix = "--",
      flagName = "csv",
      flag = FlagWithFilePath
    }

-- | Target relative standard deviation of measurements in percents (5 by default). Large values correspond to fast
-- and loose benchmarks, and small ones to long and precise. If it takes far too long, consider setting @--timeout@,
-- which will interrupt benchmarks, potentially before reaching the target deviation.
stdev :: Option ('MkFlagKind 'FlagAppearanceWithValueOrAbsent Int)
stdev =
  MkOption
    { flagPrefix = "--",
      flagName = "stdev",
      flag = FlagWithInt
    }

-- sampleReifiedOptions :: [Text]
-- sampleReifiedOptions =
--   reifyOptions
--     [ (MkReifiableOption nonmovingGC True),
--       (MkReifiableOption initialThreadStackSize (KiloBytes 100))
--     ]

-- Available options:
--   -h,--help                Show this help text
--   -p,--pattern PATTERN     Select only tests which satisfy a pattern or awk
--                            expression
--   -t,--timeout DURATION    Timeout for individual tests (suffixes: ms,s,m,h;
--                            default: s)
--   --hide-progress          Do not show progress
--   -l,--list-tests          Do not run the tests; just print their names
--   -j,--num-threads NUMBER  Number of threads to use for tests execution
--                            (default: # of cores/capabilities)
--   -q,--quiet               Do not produce any output; indicate success only by
--                            the exit code
--   --hide-successes         Do not print tests that passed successfully
--   --min-duration-to-report DURATION
--                            The minimum amount of time a test can take before
--                            tasty prints timing information (suffixes: ms,s,m,h;
--                            default: s)
--   --color never|always|auto
--                            When to use colored output (default: auto)
--   --ansi-tricks ARG        Enable various ANSI terminal tricks. Can be set to
--                            'true' or 'false'. (default: true)
--   --baseline ARG           File with baseline results in CSV format to compare
--                            against
--   --csv ARG                File to write results in CSV format
--   --svg ARG                File to plot results in SVG format
--   --stdev ARG              Target relative standard deviation of measurements in
--                            percents (5 by default). Large values correspond to
--                            fast and loose benchmarks, and small ones to long and
--                            precise. If it takes far too long, consider setting
--                            --timeout, which will interrupt benchmarks,
--                            potentially before reaching the target deviation.
--   --fail-if-slower ARG     Upper bound of acceptable slow down in percents. If a
--                            benchmark is unacceptably slower than baseline (see
--                            --baseline), it will be reported as failed.
--   --fail-if-faster ARG     Upper bound of acceptable speed up in percents. If a
--                            benchmark is unacceptably faster than baseline (see
--                            --baseline), it will be reported as failed.
--   --time-mode ARG          Whether to measure CPU time ("cpu") or wall-clock
--                            time ("wall") (default: cpu)