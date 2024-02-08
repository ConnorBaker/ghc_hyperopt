module Data.GHC.HyperOpt.Harness where

import Control.Applicative (Applicative (pure, (<*>)))
import Control.Category ((>>>))
import Control.Monad (Monad (..), join)
import Data.Bool (Bool (..))
import Data.ByteString.Lazy qualified as ByteStringLazy
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Either (Either (..))
import Data.Function ((&))
import Data.Functor (Functor (fmap), (<$>))
import Data.GHC.HyperOpt.Benchmark (Benchmark, parseBenchmarks)
import Data.GHC.HyperOpt.Options
  ( ReifiableOption (MkReifiableOption),
    reifyOptions,
  )
import Data.GHC.HyperOpt.Options.RTS qualified as RTSOptions
import Data.GHC.HyperOpt.Options.Tasty qualified as TastyOptions
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (Semigroup (..))
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Conc (STM, atomically)
import GHC.IO (FilePath)
import GHC.Num (Num (..))
import Numeric.Natural (Natural)
import System.IO (IO)
import System.Process.Typed
  ( ExitCode (..),
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

-- | Run a cabal process in a given directory with a list of arguments
runCabalProcess ::
  -- | The directory in which to run the process
  FilePath ->
  -- | The arguments to pass to the process
  [Text] ->
  -- | The exit code, stdout, and stderr of the process
  IO (ExitCode, ByteString, ByteString)
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

_lazyByteStringToText :: ByteString -> Text
_lazyByteStringToText = ByteStringLazy.toStrict >>> Text.decodeUtf8 >>> Text.strip

_stderrToLeftText :: forall a. ByteString -> IO (Either Text a)
_stderrToLeftText =
  ByteStringLazy.toStrict
    >>> Text.decodeUtf8
    >>> Left
    >>> pure

-- | Build a target in a cabal project
cabalBuild ::
  -- | Path to the directory containing the cabal project
  FilePath ->
  -- | Component to build
  Text ->
  -- | GHC options
  [ReifiableOption] ->
  -- | Parsed output or error
  IO (Either Text ())
cabalBuild cwd component ghcOpts =
  let args :: [Text]
      args = ["build", component] <> (fmap ("--ghc-option=" <>) ("-rtsopts" : reifyOptions ghcOpts))
   in runCabalProcess cwd args >>= \case
        (ExitSuccess, _, _) -> () & pure & pure
        (_, _, stderr) -> _stderrToLeftText stderr

-- | List the binary for a target in a cabal project
cabalListBin ::
  -- | Path to the directory containing the cabal project
  FilePath ->
  -- | Component to list
  Text ->
  -- | Parsed output or error
  IO (Either Text Text)
cabalListBin cwd component =
  runCabalProcess cwd ["list-bin", component] >>= \case
    (ExitSuccess, stdout, _) ->
      stdout
        & _lazyByteStringToText
        & pure
        & pure
    (_, _, stderr) -> _stderrToLeftText stderr

-- Build and benchmark a target
cabalBenchmark ::
  -- | Path to the directory containing the cabal project
  FilePath ->
  -- | Component to benchmark
  Text ->
  -- | GHC options
  [ReifiableOption] ->
  -- | RTS options
  [ReifiableOption] ->
  -- | Parsed output or error
  IO (Either Text (NonEmpty (Benchmark Natural)))
cabalBenchmark cwd component ghcOpts rtsOpts =
  let tastyOptions :: [ReifiableOption]
      tastyOptions =
        [ (MkReifiableOption TastyOptions.csv "/dev/stdout"),
          (MkReifiableOption TastyOptions.numThreads 1),
          (MkReifiableOption TastyOptions.quiet True)
        ]
      rtsPrefixOptions :: [ReifiableOption]
      rtsPrefixOptions =
        [ (MkReifiableOption RTSOptions.beginRtsOptions True),
          (MkReifiableOption RTSOptions.collectGcStatistics True)
        ]
      rtsSuffixOptions :: [ReifiableOption]
      rtsSuffixOptions = [(MkReifiableOption RTSOptions.endRtsOptions True)]
   in cabalBuild cwd component ghcOpts >>= \case
        Left err -> pure (Left err)
        Right () ->
          cabalListBin cwd component >>= \case
            Left err -> pure (Left err)
            Right bin ->
              runBenchmark
                (Text.unpack bin)
                (reifyOptions (tastyOptions <> rtsPrefixOptions <> rtsOpts <> rtsSuffixOptions))

-- TODO: Accept the current working directory as an argument
runBenchmark ::
  -- | Path to the benchmark binary
  FilePath ->
  -- | Arguments to pass to the benchmark binary
  [Text] ->
  -- | Parsed output or error
  IO (Either Text (NonEmpty (Benchmark Natural)))
runBenchmark bin args =
  let benchmarkProc :: ProcessConfig () (STM ByteString) (STM ByteString)
      benchmarkProc =
        args
          & fmap Text.unpack
          & proc bin
          & setStdout byteStringOutput
          & setStderr byteStringOutput
      gatherResults :: Process () (STM ByteString) (STM ByteString) -> IO (Either Text (NonEmpty (Benchmark Natural)))
      gatherResults p =
        waitExitCode p >>= \case
          ExitSuccess ->
            p
              & getStdout
              & atomically
              & fmap (_lazyByteStringToText >>> parseBenchmarks)
          _ ->
            p
              & getStderr
              & atomically
              & fmap _stderrToLeftText
              & join
   in withProcessWait benchmarkProc gatherResults

-- >>> runCabalProcess "/Users/connorbaker/Packages/ghc_hyperopt/FibHaskell" ["--version"]

-- >>> cabalBuild "/Users/connorbaker/Packages/ghc_hyperopt/FibHaskell" "bench:bench-fib" []

-- >>> cabalListBin "/Users/connorbaker/Packages/ghc_hyperopt/FibHaskell" "bench:bench-fib"

-- >>> cabalBenchmark "/Users/connorbaker/Packages/ghc_hyperopt/FibHaskell" "bench:bench-fib" [] []

-- >>> cabalBenchmark "/Users/connorbaker/Packages/ghc_hyperopt/FibHaskell" "bench:bench-fib" [] [ (MkReifiableOption RTSOptions.nonmovingGC True), (MkReifiableOption RTSOptions.initialThreadStackSize (KiloBytes 100)) ]
