module Data.GHC.HyperOpt.Options where

import Control.Applicative (Applicative (pure))
import Control.Monad qualified as Monad
import Data.Bool qualified as Bool
import Data.Function (($), (&), (.))
import Data.Functor (($>))
import Data.GHC.HyperOpt.Flags
  ( Flag (..),
    FlagKind (MkFlagKind),
    FlagPresent (..),
    FlagWithValue (..),
    FlagWithValueKind (..), flagValueHandler,
  )
import Data.Kind (Type)
import Data.Maybe (Maybe)
import Data.Maybe qualified as Maybe
import Data.Semigroup ((<>))
import Data.String (IsString (fromString), String)
import Data.Text (Text)
import Data.Traversable (Traversable (traverse))
import Text.Show (Show (show))

-- Generalizes over `Flag`, allowing us to pattern match on the GADT.
data Option :: FlagKind -> Type where
  MkOption ::
    forall (flagPresent :: FlagPresent) (a :: Type).
    (Show a) =>
    { -- | The prefix of the flag, e.g. "--", "-", or "-f"
      flagPrefix :: Text,
      -- | The name of the flag, e.g. "nonmoving-gc"
      flagName :: Text,
      -- | The flag itself, e.g. `FlagPresentOrAbsent`
      flag :: Flag ('MkFlagKind flagPresent a)
    } ->
    Option ('MkFlagKind flagPresent a)

-- | Wraps an option with its value using an existential type.
data ReifiableOption where
  MkReifiableOption ::
    forall (flagPresent :: FlagPresent) (a :: Type).
    {option :: Option ('MkFlagKind flagPresent a), value :: a} ->
    ReifiableOption

-- Pattern matching on the GADT brings the equality constraint into scope.
-- As such, we know that the `a` in `Option k a` is the same as the `a` being passed in.
reifyOption :: ReifiableOption -> Maybe Text
reifyOption (MkReifiableOption (MkOption {..}) value) =
  let -- \| Helper to format a string value.
      withStringValueFormatter ::
        forall (flagWithValue :: FlagWithValue).
        String ->
        FlagWithValueKind flagWithValue ->
        Maybe Text
      withStringValueFormatter valueString fwv =
        valueString
          & fromString
          & (flagValueHandler fwv <>)
          & (flagName <>)
          & (flagPrefix <>)
          & pure

      -- \| Helper to format a non-string value.
      withValueFormatter ::
        forall (flagWithValue :: FlagWithValue) (a :: Type).
        (Show a) =>
        a ->
        FlagWithValueKind flagWithValue ->
        Maybe Text
      withValueFormatter = withStringValueFormatter . show
   in case flag of
        -- Create the flag if it is set
        FlagSettable -> Monad.guard value $> flagPrefix <> flagName
        -- Create the flag, negating it if it is unset
        FlagNegatable -> pure $ flagPrefix <> Bool.bool "" "no-" value <> flagName
        -- Numeric flags
        FlagBool duh -> withValueFormatter value duh
        FlagInt duh -> withValueFormatter value duh
        FlagDouble duh -> withValueFormatter value duh
        FlagSize duh -> withValueFormatter value duh
        -- String flags
        FlagString duh -> withStringValueFormatter value duh

-- Options can override earlier options, so we must accept a list of options.
reifyOptions :: [ReifiableOption] -> [Text]
reifyOptions = Maybe.fromMaybe [] . traverse reifyOption
