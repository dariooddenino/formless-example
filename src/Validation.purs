module Validation where

import Prelude

import Data.Array (elem)
import Data.Either (Either(..), either, fromRight)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String (null)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flags
import Data.Traversable (class Traversable, sequence, traverse)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Formless (Validation(..), hoistFnE_, hoistFnME_, runValidation)
import Partial.Unsafe (unsafePartial)

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

-- | This is a wrapper data type we will use to represent
-- | validations in progress for async fields.
data RemoteField a = Validating a | NotValidating a
derive instance genericRemoteField :: Generic (RemoteField a) _
instance eqRemoteField :: Eq a => Eq (RemoteField a) where
  eq = genericEq
instance functorRemoteField :: Functor RemoteField where
  map f (Validating a) = Validating $ f a
  map f (NotValidating a) = NotValidating $ f a
instance foldableRemoteField :: Foldable RemoteField where
  foldr f z (NotValidating a) = a `f` z
  foldr f z (Validating a) = a `f` z
  foldl f z (NotValidating a) = z `f` a
  foldl f z (Validating a) = z `f` a
  foldMap f (NotValidating a) = f a
  foldMap f (Validating a) = f a
instance traversableRemoteField :: Traversable RemoteField where
  traverse f (Validating a) = Validating <$> f a
  traverse f (NotValidating a) = NotValidating <$> f a
  sequence (Validating x) = Validating <$> x
  sequence (NotValidating x) = NotValidating <$> x

-- | Just a function to extract the value.
unRemoteField :: forall a. RemoteField a -> a
unRemoteField (Validating a) = a
unRemoteField (NotValidating a) = a

-- | Is the field undergoing validation?
isValidating :: forall a. RemoteField a -> Boolean
isValidating = case _ of
  Validating _ -> true
  _ -> false

-- | Toggles the validating status of a field.
toggleValidation :: forall a. RemoteField a -> RemoteField a
toggleValidation (Validating a) = NotValidating a
toggleValidation (NotValidating a) = Validating a

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

-- | Check that a username is inserted, and that it's made of letters only.
-- validUserName :: forall form m. Monad m => Validation form m FieldError (RemoteField String) (RemoteField String)
-- validUserName = hoistFnE_ $ \f ->
--   let s = unRemoteField f in
--     if null s
--     then Left Missing
--     else if Regex.test userNameRegex s
--          then Right f
--          else Left InvalidUserName
validUserName :: forall form m. Monad m => Validation form m FieldError String String
validUserName = hoistFnE_ $ \s ->
    if null s
    then Left Missing
    else if Regex.test userNameRegex s
         then Right s
         else Left InvalidUserName

overRemoteField ::
  forall form m i o.
  Monad m =>
  Validation form m FieldError i o ->
  Validation form m FieldError (RemoteField i) (RemoteField o)
overRemoteField validator = Validation \form i -> do
  ((<$>) sequence) $ traverse (runValidation validator form) i

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

-- | This functions performs a (fake) effectful check to verify that
-- | the userName is not already in use.
availableUserName :: forall form. Validation form Aff FieldError (RemoteField String) (RemoteField String)
availableUserName = hoistFnME_ $ \f -> do
  let userName = unRemoteField f
  isUsed <- checkUserName userName
  pure $
    if isUsed
    then Left ExistingUserName
    else Right f

-- | This is supposed to check whether the userName
-- | is already present in the db.
checkUserName :: String -> Aff Boolean
checkUserName u = do
  delay $ Milliseconds 1000.0
  pure $ elem u [ "admin", "formless", "user" ]
