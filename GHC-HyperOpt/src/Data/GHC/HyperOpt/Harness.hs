module Data.GHC.HyperOpt.Harness where

import Control.Applicative (Applicative (pure, (<*>)))
import Control.Monad (Monad (return, (>>=)))
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Function ((&))
import Data.Functor (Functor (fmap), (<$>))
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Conc (STM, atomically)
import GHC.IO (FilePath)
import System.IO (IO)
import System.Process.Typed
  ( ExitCode,
    Process,
    ProcessConfig,
    byteStringOutput,
    getStderr,
    getStdout,
    proc,
    setStderr,
    setStdout,
    setWorkingDir,
    waitExitCode,
    withProcessWait,
  )

runCabalProcess :: FilePath -> [Text] -> IO (ExitCode, ByteString, ByteString)
runCabalProcess cwd args =
  let cabalProc :: ProcessConfig () (STM ByteString) (STM ByteString)
      cabalProc =
        args
          & fmap Text.unpack
          & proc "cabal"
          & setWorkingDir cwd
          & setStdout byteStringOutput
          & setStderr byteStringOutput
      gatherResults :: Process () (STM ByteString) (STM ByteString) -> IO (ExitCode, ByteString, ByteString)
      gatherResults p = do
        ec <- waitExitCode p
        atomically ((ec,,) <$> getStdout p <*> getStderr p)
   in withProcessWait cabalProc gatherResults

-- Build and benchmark a target
-- cabalBenchmark :: FilePath -> [Text] -> IO (ExitCode, ByteString, ByteString)
-- cabalBenchmark cwd _args = 
--   let 
--     args = 
--       [
--       "bench"
--       "--benchmark-options=--csv"

--       ] ++ _args
      
--       runCabalProcess cwd . ("bench" :)
--   in 
  

-- >>> runCabalProcess "/Users/connorbaker/Packages/ghc_hyperopt/FibHaskell" ["--version"]
-- (ExitSuccess,"cabal-install version 3.10.2.1\ncompiled using version 3.10.2.1 of the Cabal library \n","")

-- >>> cabalBenchmark "/Users/connorbaker/Packages/ghc_hyperopt/FibHaskell" ["bench:bench-fib"]
-- (ExitSuccess,"Build profile: -w ghc-9.8.1 -O1\nIn order, the following will be built (use -v for more details):\n - FibHaskell-0.1.0.0 (bench:bench-fib) (ephemeral targets)\nPreprocessing benchmark 'bench-fib' for FibHaskell-0.1.0.0..\nBuilding benchmark 'bench-fib' for FibHaskell-0.1.0.0..\nRunning 1 benchmarks...\nBenchmark bench-fib: RUNNING...\nAll\n  Fibonacci numbers\n    fifth:     OK\n      39.6 ns \194\177 2.8 ns\n    tenth:     OK\n      494  ns \194\177  26 ns\n    twentieth: OK\n      60.7 \206\188s \194\177 3.3 \206\188s\n\nAll 3 tests passed (6.80s)\nBenchmark bench-fib: FINISH\n","")
