module Data.GHC.HyperOpt.Flags where

import Data.Bool (Bool)
import Data.Int (Int)
import Data.Kind (Type)
import Data.Semigroup ((<>))
import Data.String (IsString (fromString), String)
import GHC.Float (Double)
import Text.Show (Show (show))
import System.IO (FilePath)

data Size :: Type where
  Bytes :: Int -> Size
  KiloBytes :: Int -> Size
  MegaBytes :: Int -> Size
  GigaBytes :: Int -> Size

instance Show Size where
  show :: Size -> String
  show (Bytes n) = show n
  show (KiloBytes n) = show n <> "k"
  show (MegaBytes n) = show n <> "m"
  show (GigaBytes n) = show n <> "g"


data FlagAppearance :: Type where
  FlagAppearanceSetOrUnset :: FlagAppearance
  FlagAppearanceWithoutValueOrAbsent :: FlagAppearance
  FlagAppearanceWithValueOrAbsent :: FlagAppearance

-- | The kind of a flag, which contains the name and type of the flag.
data FlagKind :: Type where
  MkFlagKind :: {flagAppearance :: FlagAppearance, flagType :: Type} -> FlagKind

-- Each constructor is indexed by a 'FlagKind' which contains the name and type of the flag.
data Flag :: FlagKind -> Type where
  FlagSetOrUnset :: Flag ('MkFlagKind 'FlagAppearanceSetOrUnset Bool)
  FlagWithoutValueOrAbsent :: Flag ('MkFlagKind 'FlagAppearanceWithoutValueOrAbsent Bool)
  FlagWithInt :: Flag ('MkFlagKind 'FlagAppearanceWithValueOrAbsent Int)
  FlagWithDouble :: Flag ('MkFlagKind 'FlagAppearanceWithValueOrAbsent Double)
  FlagWithSize :: Flag ('MkFlagKind 'FlagAppearanceWithValueOrAbsent Size)
  FlagWithFilePath :: Flag ('MkFlagKind 'FlagAppearanceWithValueOrAbsent FilePath)
