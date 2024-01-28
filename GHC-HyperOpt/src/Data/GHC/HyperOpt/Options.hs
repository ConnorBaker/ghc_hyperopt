module Data.GHC.HyperOpt.Options where

import Control.Applicative (Applicative (pure))
import Control.Monad qualified as Monad
import Data.Bool qualified as Bool
import Data.Function (($), (.))
import Data.Functor (($>))
import Data.GHC.HyperOpt.Flags
  ( Flag (..),
    FlagAppearance,
    FlagKind (MkFlagKind),
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
    forall (appearance :: FlagAppearance) (a :: Type).
    (Show a) =>
    { -- | The prefix of the flag, e.g. "--", "-", or "-f"
      flagPrefix :: Text,
      -- | The name of the flag, e.g. "nonmoving-gc"
      flagName :: Text,
      -- | The flag itself, e.g. `FlagPresentOrAbsent`
      flag :: Flag ('MkFlagKind appearance a)
    } ->
    Option ('MkFlagKind appearance a)
  

-- | Wraps an option with its value using an existential type.
data ReifiableOption where
  MkReifiableOption ::
    forall (appearance :: FlagAppearance) (a :: Type).
    {option :: Option ('MkFlagKind appearance a), value :: a} ->
    ReifiableOption

-- Pattern matching on the GADT brings the equality constraint into scope.
-- As such, we know that the `a` in `Option k a` is the same as the `a` being passed in.
reifyOption :: ReifiableOption -> Maybe Text
reifyOption (MkReifiableOption (MkOption {..}) value) =
  let withStringValueFormatter :: String -> Maybe Text
      withStringValueFormatter = pure . ((flagPrefix <> flagName) <>) . fromString
      withValueFormatter :: (Show a) => a -> Maybe Text
      withValueFormatter = withStringValueFormatter . show
   in case flag of
        FlagWithoutValueOrAbsent -> Monad.guard value $> flagPrefix <> flagName
        FlagSetOrUnset -> pure $ flagPrefix <> Bool.bool "" "no-" value <> flagName
        FlagWithFilePath -> withStringValueFormatter value
        FlagWithInt -> withValueFormatter value
        FlagWithDouble -> withValueFormatter value
        FlagWithSize -> withValueFormatter value

-- Options can override earlier options, so we must accept a list of options.
reifyOptions :: [ReifiableOption] -> [Text]
reifyOptions = Maybe.fromMaybe [] . traverse reifyOption
