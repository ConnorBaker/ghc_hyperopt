module ParseBenchmarkTests where

import Data.GHC.HyperOpt.Benchmark
  ( Benchmark (..),
    BenchmarkMemoryResults
      ( BenchmarkMemoryResults,
        allocated,
        copied,
        peak
      ),
    BenchmarkResults (BenchmarkResults, memory, time),
    BenchmarkTimeResults (BenchmarkTimeResults, mean, stdev),
    parseBenchmarks,
  )
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit as HU (testCase, (@?=))

csvHeader :: Text
csvHeader = "Name,Mean (ps),2*Stdev (ps),Allocated,Copied,Peak Memory"

sampleCSVLineOne :: Text
sampleCSVLineOne = "All.Fibonacci numbers.fifth,38314,3242,223,0,6291456"

sampleCSVLineTwo :: Text
sampleCSVLineTwo = "All.Fibonacci numbers.tenth,486292,26446,2319,0,6291456"

sampleCSVLineThree :: Text
sampleCSVLineThree = "All.Fibonacci numbers.twentieth,59362792,3366112,283340,20,6291456"

sampleOne :: Text
sampleOne =
  T.unlines
    [ csvHeader,
      sampleCSVLineOne
    ]

expectedOne :: Either Text (NonEmpty (Benchmark Natural))
expectedOne =
  Right
    ( Benchmark
        { name = "All.Fibonacci numbers.fifth",
          results =
            BenchmarkResults
              { time =
                  BenchmarkTimeResults
                    { mean = 38314,
                      stdev = 3242
                    },
                memory =
                  BenchmarkMemoryResults
                    { allocated = 223,
                      copied = 0,
                      peak = 6291456
                    }
              }
        }
        :| []
    )

sampleTwo :: Text
sampleTwo =
  T.unlines
    [ csvHeader,
      sampleCSVLineTwo
    ]

expectedTwo :: Either Text (NonEmpty (Benchmark Natural))
expectedTwo =
  Right
    ( Benchmark
        { name = "All.Fibonacci numbers.tenth",
          results =
            BenchmarkResults
              { time =
                  BenchmarkTimeResults
                    { mean = 486292,
                      stdev = 26446
                    },
                memory = BenchmarkMemoryResults {allocated = 2319, copied = 0, peak = 6291456}
              }
        }
        :| []
    )

sampleThree :: Text
sampleThree =
  T.unlines
    [ csvHeader,
      sampleCSVLineThree
    ]

expectedThree :: Either Text (NonEmpty (Benchmark Natural))
expectedThree =
  Right
    ( Benchmark
        { name = "All.Fibonacci numbers.twentieth",
          results =
            BenchmarkResults
              { time =
                  BenchmarkTimeResults
                    { mean = 59362792,
                      stdev = 3366112
                    },
                memory =
                  BenchmarkMemoryResults
                    { allocated = 283340,
                      copied = 20,
                      peak = 6291456
                    }
              }
        }
        :| []
    )

sampleBulk :: Text
sampleBulk =
  T.unlines
    [ csvHeader,
      sampleCSVLineOne,
      sampleCSVLineTwo,
      sampleCSVLineThree
    ]

expectedBulk :: Either Text (NonEmpty (Benchmark Natural))
expectedBulk = foldl (liftA2 (<>)) expectedOne [expectedTwo, expectedThree]

tests :: TestTree
tests =
  testGroup
    "Test benchmark parsing"
    [ HU.testCase "parsing singleton one" $
        parseBenchmarks sampleOne @?= expectedOne,
      HU.testCase "parsing singleton two" $
        parseBenchmarks sampleTwo @?= expectedTwo,
      HU.testCase "parsing singleton three" $
        parseBenchmarks sampleThree @?= expectedThree,
      HU.testCase "parsing bulk" $
        parseBenchmarks sampleBulk @?= expectedBulk
    ]
