module Data.GHC.HyperOpt.Benchmark where

import Control.Arrow ((<<<), (>>>))
import Control.Monad (join)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as T
import GHC.Generics (Generic1, Generically1 (..))
import Numeric.Natural (Natural)
import Prelude

data BenchmarkTimeResults a where
  BenchmarkTimeResults ::
    { mean :: a,
      stdev :: a
    } ->
    BenchmarkTimeResults a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic1)
  deriving (Applicative) via Generically1 BenchmarkTimeResults

data BenchmarkMemoryResults a where
  BenchmarkMemoryResults ::
    { allocated :: a,
      copied :: a,
      peak :: a
    } ->
    BenchmarkMemoryResults a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic1)
  deriving (Applicative) via Generically1 BenchmarkMemoryResults

data BenchmarkResults a where
  BenchmarkResults ::
    { time :: BenchmarkTimeResults a,
      memory :: BenchmarkMemoryResults a
    } ->
    BenchmarkResults a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic1)
  deriving (Applicative) via Generically1 BenchmarkResults

data Benchmark a where
  Benchmark ::
    { name :: Text,
      results :: BenchmarkResults a
    } ->
    Benchmark a
  deriving stock (Eq, Ord, Show)

-- TODO: Make this more flexible with respect to the ordering of the fields
-- NOTE: The test name can be an arbitrary string, so it may contain commas
-- However, the other fields are numbers, so they cannot contain commas.
-- We reverse the list of fields, split on the first comma until we
-- have the desired number of fields, and then reverse the list again.
parseBenchmarks :: Text -> Either Text (NonEmpty (Benchmark Natural))
parseBenchmarks =
  let checkHeader :: Text -> Maybe Text
      checkHeader = \case
        "Name,Mean (ps),2*Stdev (ps),Allocated,Copied,Peak Memory" -> Nothing
        _ -> Just "Header does not match expected header"

      textToNatural :: Text -> Either Text Natural
      textToNatural =
        T.decimal >>> \case
          Left s -> Left $ "Could not parse number: " <> fromString s
          Right (n, "") -> Right n
          Right (n, rest) -> Left $ "Could not parse number: " <> T.pack (show n) <> "; had non-empty rest " <> rest

      doFieldSplit :: Text -> [Text]
      doFieldSplit = T.reverse >>> T.split (== ',') >>> fmap T.reverse

      undoFieldSplit :: [Text] -> Text
      undoFieldSplit = T.reverse <<< T.intercalate "," <<< fmap T.reverse

      parseLine :: Text -> Either Text (Benchmark Natural)
      parseLine =
        doFieldSplit >>> \case
          (peak : copied : allocated : stdev : mean : _name) -> do
            Benchmark
              <$> pure (undoFieldSplit _name) -- Need to reverse each possible component of the name and join them
              <*> traverse
                textToNatural
                BenchmarkResults
                  { time = BenchmarkTimeResults {..},
                    memory = BenchmarkMemoryResults {..}
                  }
          invalid -> Left $ "Line does not have 6 fields: " <> undoFieldSplit invalid

      getBody :: Text -> Either Text (NonEmpty Text)
      getBody =
        T.lines >>> \case
          [] -> Left "Empty input"
          [actualHeader] -> maybe (Right (error "No body")) Left (checkHeader actualHeader)
          (actualHeader : x : y) -> maybe (Right (x :| y)) Left (checkHeader actualHeader)
   in getBody >>> fmap (traverse parseLine) >>> join
