module Main where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Formless (Validation(..), runValidation)
import Formless as F
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Validation (FieldError, RemoteField(..), acceptEmpty, availableUserName, isValidating, nonEmpty, overRemoteField, showError, toggleValidation, validEmail, validPhone, validUserName)

type Input = Unit
type State = Unit

data Query a
  = HandleFormless (F.Message' Form) a

type Message = Void

type ChildQuery = F.Query' Form Aff
type ChildSlot = Unit

-- | This is the component that will hold the form.
-- | In this example we will keep it as simple as possible.
component :: H.Component HH.HTML Query Input Message Aff
component =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }

    where

      initialState = identity

      render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
      render state =
        HH.div_
        [ HH.slot unit F.component { initialInputs
                                   , validators
                                   , render: renderForm
                                   } (HE.input HandleFormless)
        ]

      eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message Aff
      eval (HandleFormless _ next) = pure next

-- | Our form will have three fields.
-- | The userName will perform a fake async validation.
-- | The email field is mandatory only when `sendEmail` is checked.
-- | We are also checking that it's in the correct format.
-- | The phone field is mandatory and will have to be in the correct format.
newtype Form r f = Form (r ( userName :: f FieldError (RemoteField String) (RemoteField String)
                           , email :: f FieldError String (Maybe String)
                           , phone :: f FieldError String String
                           , sendEmail :: f FieldError Boolean Boolean
                           ))
derive instance newtypeForm :: Newtype (Form r f) _

-- | prx is a record that holds proxies for all our fields,
-- | so that we don't have to redefine them every time we need them.
prx :: F.SProxies Form
prx = F.mkSProxies $ F.FormProxy :: F.FormProxy Form

-- | Our initial inputs are all empty.
initialInputs :: Form Record F.InputField
--initialInputs = F.mkInputFields (F.FormProxy :: F.FormProxy Form)
initialInputs = F.wrapInputFields
  { userName: NotValidating ""
  , email: ""
  , phone: ""
  , sendEmail: false
  }

-- | Here we are using the validators we defined.
-- | sendEmail has no validation.
validators :: Form Record (F.Validation Form Aff)
validators = Form
  { userName: overRemoteField validUserName >>> availableUserName
  , email: emailValidator
  , phone: nonEmpty >>> validPhone
  , sendEmail: F.hoistFn_ identity
  }
  where
    -- | This is the validator that we will use for our email field.
    -- | If the sendEmail checkbox is not checked, this runs the `validEmail`
    -- | validator only when the field is not empty.
    -- | If the checkbox is checked, the email field becomes mandatory.
    emailValidator ::
      forall m.
      Monad m =>
      Validation Form m FieldError String (Maybe String)
    emailValidator = Validation $ \form email ->
      if F.getInput prx.sendEmail form
      then runValidation (nonEmpty >>> validEmail >>> F.hoistFn_ Just) form email
      else runValidation (acceptEmpty validEmail) form email

-- | Renders the error when necessary.
renderError :: forall i p. Maybe String -> HH.HTML i p
renderError Nothing = HH.p_ [ HH.text "" ]
renderError (Just e) = HH.p [ HP.class_ $ ClassName "help is-danger" ] [ HH.text e ]

-- | This is the function that renders our form.
renderForm :: F.State Form Aff -> F.HTML' Form Aff
renderForm { form } =
  HH.div_
  [ HH.div
    [ HP.class_ $ ClassName "field" ]
    [ HH.label
      [ HP.class_ $ ClassName "label" ]
      [ HH.text $ "User name" ]
    , HH.div
      [ HP.class_ $ ClassName "control" ]
      [ HH.input [ HP.type_ HP.InputText
                 , HP.class_ $ ClassName "input"
                 , HP.required true
                 , HP.disabled $ isValidating $ F.getInput prx.userName form
                 , HE.onValueInput $ HE.input $ F.set prx.userName <<< NotValidating
                   -- on blur we first set the field to validating to disable the input
                   -- and then run the validation on the nonvalidating field so that it's enabled
                   -- again at the end
                 , HE.onBlur $ HE.input_ $
                     (F.modify_ prx.userName toggleValidation) `F.andThen` (F.modifyValidate_ prx.userName toggleValidation)
                 ]
      , renderError $ showError $ F.getResult prx.userName form
      ]
    ]
  , HH.div
    [ HP.class_ $ ClassName "field" ]
    [ HH.label
      [ HP.class_ $ ClassName "label" ]
      [ HH.text $ "Email" ]
    , HH.div
      [ HP.class_ $ ClassName "control" ]
      [ HH.input [ HP.type_ HP.InputText
                 , HP.class_ $ ClassName "input"
                 , HP.required $ F.getInput prx.sendEmail form
                 , HE.onValueInput $ HE.input $ F.setValidate prx.email
                 ]
      , renderError $ showError $ F.getResult prx.email form
      ]
    ]
  , HH.div
    [ HP.class_ $ ClassName "field" ]
    [ HH.label
      [ HP.class_ $ ClassName "label" ]
      [ HH.text $ "Phone" ]
    , HH.div
      [ HP.class_ $ ClassName "control" ]
      [ HH.input [ HP.type_ HP.InputText
                 , HP.class_ $ ClassName "input"
                 , HP.required true
                 , HE.onValueInput $ HE.input $ F.setValidate prx.phone
                 ]
      , renderError $ showError $ F.getResult prx.phone form
      ]
    ]
  , HH.div
    [ HP.class_ $ ClassName "field" ]
    [ HH.label
      [ HP.class_ $ ClassName "checkbox" ]
      [ HH.input [ HP.type_ HP.InputCheckbox
                   -- When the value changes, we retrigger the email field validation.
                 , HE.onChecked $ HE.input $ \b ->
                   (F.setValidate_ prx.sendEmail b) `F.andThen` (F.modifyValidate_ prx.email identity)
                 ]
      , HH.text $ "Send me spam emails"
      ]
    ]
  ]

main :: Effect Unit
main = do
  HA.runHalogenAff do
    HA.awaitLoad
    HA.selectElement (wrap "#form") >>= traverse_ \domElement ->
      runUI component unit domElement
