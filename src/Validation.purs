module Validation where

import Prelude

import Data.Either (Either(..), either, fromRight)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (null)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flags
import Formless (FormField, Validation(..), hoistFnE_, runValidation)
import Formless as F
import Partial.Unsafe (unsafePartial)
import Type.Prelude (SProxy(..))

-- | The possible errors that can appear in our form.
data FieldError
  = InvalidEmail
  | InvalidPhone
  | InvalidUserName
  | ExistingUserName
  | Missing

derive instance genericFieldError :: Generic FieldError _

instance showFieldError :: Show FieldError where
  show = genericShow

-- | We want to show some meaningful text to the user,
-- | so we can't just use the output of `genericShow`.
errorToString :: FieldError -> String
errorToString = case _ of
  InvalidEmail -> "The email is not valid."
  InvalidPhone -> "The phone number is not valid."
  InvalidUserName -> "Your user name can contain letters only."
  ExistingUserName -> "The user name you chose is not available."
  Missing -> "This field is required."

-- | A function that renders an error if present.
showError :: forall o. Maybe (Either FieldError o) -> Maybe String
showError = (=<<) (either (pure <<< errorToString) (const Nothing))

-- | It's unsafe, but we are using it only for hard-coded
-- | and tested regexes, so it's good enough for this example.
unsafeRegexFromString :: String -> Regex.Regex
unsafeRegexFromString str =
  let regex = Regex.regex str Regex.Flags.noFlags
  in unsafePartial (fromRight regex)

-- | A regex to test whether the email is in the correct format.
emailRegex :: Regex.Regex
emailRegex = unsafeRegexFromString "^\\w+([.-]?\\w+)*@\\w+([.-]?\\w+)*(\\.\\w{2,3})+$"

-- | A regex to test whether the phone number is in the correct format.
phoneRegex :: Regex.Regex
phoneRegex = unsafeRegexFromString "^([\\(\\)\\+0-9\\s\\-\\#]+)$"

-- | Only letters are allowed in usernames.
userNameRegex :: Regex.Regex
userNameRegex = unsafeRegexFromString "^([a-zA-Z]+)$"

-- | We will use this validator to check whether fields are empty.
nonEmpty :: forall form m. Monad m => Validation form m FieldError String String
nonEmpty = hoistFnE_ $ \str ->
  if null str
  then Left Missing
  else Right str

-- | This is the function that we will use to validate emails.
validEmail :: forall form m. Monad m => Validation form m FieldError String String
validEmail = hoistFnE_ $ \str ->
  if Regex.test emailRegex str
  then Right str
  else Left InvalidEmail

-- | This is the function that we will use to validate phone numbers.
validPhone :: forall form m. Monad m => Validation form m FieldError String String
validPhone = hoistFnE_ $ \str ->
  if Regex.test phoneRegex str
  then Right str
  else Left InvalidPhone

-- | We can think of this as a higher-order validator.
-- | It takes a validator as input, and runs it only when
-- | the input is not empty.
acceptEmpty ::
  forall form m o.
  Monad m =>
  Validation form m FieldError String o ->
  Validation form m FieldError String (Maybe o)
acceptEmpty validator = Validation \form str ->
  if null str
  then pure $ Right Nothing
  else do
    res <- runValidation validator form str
    pure $ Just <$> res

