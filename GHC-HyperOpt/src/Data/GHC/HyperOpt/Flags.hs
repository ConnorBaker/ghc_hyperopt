module Data.GHC.HyperOpt.Flags where

import Data.Bool (Bool)
import Data.Int (Int)
import Data.Kind (Type)
import Data.Semigroup ((<>))
import Data.String (IsString (fromString), String)
import GHC.Float (Double)
import Text.Show (Show (show))
import Data.Text (Text)

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

-- Assume that a flag is present (that is, we are not interested in absent flags).
-- If a flag is present, it can appear either with or without a value.
data FlagPresent :: Type where
  WithoutValue :: FlagPresent
  WithValue :: FlagWithValue -> FlagPresent

-- If a flag has a value, it may appear either immediately after the flag, after an "=" sign, or after a space.
data FlagWithValue :: Type where
  ImmediatelyAfterFlag :: FlagWithValue
  AfterEqualsSign :: FlagWithValue
  AfterSpace :: FlagWithValue

data FlagWithValueKind :: FlagWithValue -> Type where
  ImmediatelyAfterFlagKind :: FlagWithValueKind 'ImmediatelyAfterFlag
  AfterEqualsSignKind :: FlagWithValueKind 'AfterEqualsSign
  AfterSpaceKind :: FlagWithValueKind 'AfterSpace

-- | The kind of a flag, which contains the name and type of the flag.
data FlagKind :: Type where
  MkFlagKind :: {flagPresent :: FlagPresent, a :: Type} -> FlagKind

data Flag :: FlagKind -> Type where
  -- | A flag that is present and has no value.
  FlagSettable :: Flag ('MkFlagKind 'WithoutValue Bool)
  -- | A flag that is present, has no value, and can either be set or negated (e.g. --foo or --no-foo)
  FlagNegatable :: Flag ('MkFlagKind 'WithoutValue Bool)
  -- | A flag that is present and has a boolean value.
  FlagBool :: forall (flagWithValue :: FlagWithValue). FlagWithValueKind flagWithValue -> Flag ('MkFlagKind ('WithValue flagWithValue) Bool)
  -- | A flag that is present and has an integer value.
  FlagInt :: forall (flagWithValue :: FlagWithValue). FlagWithValueKind flagWithValue -> Flag ('MkFlagKind ('WithValue flagWithValue) Int)
  -- | A flag that is present and has a double value.
  FlagDouble :: forall (flagWithValue :: FlagWithValue). FlagWithValueKind flagWithValue -> Flag ('MkFlagKind ('WithValue flagWithValue) Double)
  -- | A flag that is present and has a size value.
  FlagSize :: forall (flagWithValue :: FlagWithValue). FlagWithValueKind flagWithValue -> Flag ('MkFlagKind ('WithValue flagWithValue) Size)
  -- | A flag that is present and has a string value.
  FlagString :: forall (flagWithValue :: FlagWithValue). FlagWithValueKind flagWithValue -> Flag ('MkFlagKind ('WithValue flagWithValue) String)

flagValueHandler :: forall (flagWithValue :: FlagWithValue). FlagWithValueKind flagWithValue -> Text
flagValueHandler = \case
  ImmediatelyAfterFlagKind -> ""
  AfterEqualsSignKind -> "="
  AfterSpaceKind -> " "
