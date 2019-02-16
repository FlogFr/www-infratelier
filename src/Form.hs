module Form where

import Protolude
import Data
import Data.String (String)
import Servant.Multipart


-- | Lookup a textual input with the given @name@ attribute.
lookupInputs :: Text -> MultipartData tag -> Either String [Text]
lookupInputs iname = Right . fmap iValue . filter ((==iname) . iName) . inputs


class FormVerifiable a where
  checkForm :: a -> [Message]

data EmailForm = EmailForm {
    emailFormEmail       :: Text
  , emailFormSecretValue :: Text
  } deriving (Show)

data NewsletterForm = NewsletterForm {
    newsletterFormEmail :: Text
  , newsletterFormSecretValue :: Text
  } deriving (Generic, Show)
instance FromMultipart Mem NewsletterForm where
  fromMultipart multipartData =
    NewsletterForm <$> lookupInput "email" multipartData
                   <*> lookupInput "secret-value" multipartData

data ContactForm = ContactForm {
    contactFormEmail :: Text
  , contactFormSecretValue :: Text
  , contactFormMessage :: Text
  } deriving (Generic, Show)
instance FromMultipart Mem ContactForm where
  fromMultipart multipartData =
    ContactForm <$> lookupInput "email" multipartData
                <*> lookupInput "secret-value" multipartData
                <*> lookupInput "message" multipartData

instance FromMultipart Mem EmailForm where
  fromMultipart multipartData =
    EmailForm <$> lookupInput "email" multipartData
              <*> lookupInput "secret-value" multipartData

data UserLogin = UserLogin {
    userLoginEmail :: Text
  , userLoginPassword :: Text
  } deriving (Show)

instance FromMultipart Mem UserLogin where
  fromMultipart multipartData =
    UserLogin <$> lookupInput "email" multipartData
              <*> lookupInput "password" multipartData

data MenuIdForm = MenuIdForm {
    menuIdFormId :: Text
  } deriving (Generic, Show)

instance FromMultipart Mem MenuIdForm where
  fromMultipart multipartData =
    MenuIdForm <$> lookupInput "menuId" multipartData

data ResetForm = ResetForm {
    resetFormEmail :: Text
  , resetFormEmailConfirm :: Text
  , resetFormSecretValue :: Text
  } deriving (Generic, Show)
instance FromMultipart Mem ResetForm where
  fromMultipart multipartData =
    ResetForm <$> lookupInput "email" multipartData
              <*> lookupInput "email2" multipartData
              <*> lookupInput "secret-value" multipartData

data RegisterForm = RegisterForm {
    registerFormEmail :: Text
  , registerFormPassword :: Text
  , registerFormPasswordConfirm :: Text
  , registerFormFirstName :: Text
  , registerFormLastName :: Text
  , registerFormNewsletter :: Either String Text
  } deriving (Generic, Show)
instance FromMultipart Mem RegisterForm where
  fromMultipart multipartData =
    RegisterForm <$> lookupInput "email" multipartData
                 <*> lookupInput "password" multipartData
                 <*> lookupInput "password2" multipartData
                 <*> lookupInput "first-name" multipartData
                 <*> lookupInput "last-name" multipartData
                 <*> Right (lookupInput "newsletter" multipartData)

data RegisterProForm = RegisterProForm {
    leadInput :: Text
  , confirm :: Either String Text
  } deriving (Generic, Show)
instance FromMultipart Mem RegisterProForm where
  fromMultipart multipartData =
    RegisterProForm <$> lookupInput "leadInput" multipartData
                    <*> Right (lookupInput "confirm" multipartData)

data EventForm = EventForm {
    eventFormTitle       :: Text
  , eventFormUrl         :: Text
  , eventFormDescription :: Text
  , eventFormStartTime   :: Text
  , eventFormStopTime    :: Text
  , eventFormPositionLat :: Text
  , eventFormPositionLng :: Text
  } deriving (Generic, Show)

instance FromMultipart Mem EventForm where
  fromMultipart multipartData =
    EventForm <$> lookupInput "title" multipartData
              <*> lookupInput "url" multipartData
              <*> lookupInput "description" multipartData
              <*> lookupInput "datetime-start" multipartData
              <*> lookupInput "datetime-end" multipartData
              <*> lookupInput "lat" multipartData
              <*> lookupInput "lng" multipartData

data MenuForm = MenuForm {
    menuFormTitle :: Text
  , menuFormDescription :: Text
  , menuFormUrl :: Text
  , menuFormCategories :: [Text]
  , menuFormLineTitle :: [Text]
  , menuFormLinePrice :: [Text]
  } deriving (Generic, Show)

instance FromMultipart Mem MenuForm where
  fromMultipart multipartData =
    MenuForm <$> lookupInput "title" multipartData
                     <*> lookupInput "description" multipartData
                     <*> lookupInput "url" multipartData
                     <*> lookupInputs "category" multipartData
                     <*> lookupInputs "menuline-title" multipartData
                     <*> lookupInputs "menuline-price" multipartData


data MenuLocalizeForm = MenuLocalizeForm {
    menuLocalizeLat :: Text
  , menuLocalizeLng :: Text
  , menuLocalizeDuration :: Text
  } deriving (Generic, Show)

instance FromMultipart Mem MenuLocalizeForm where
  fromMultipart multipartData =
    MenuLocalizeForm <$> lookupInput "lat" multipartData
                     <*> lookupInput "lng" multipartData
                     <*> lookupInput "duration" multipartData

data MenuPlanForm = MenuPlanForm {
    menuPlanLat :: [Text]
  , menuPlanLng :: [Text]
  , menuPlanTimezone :: [Text]
  , menuPlanDatetimeStart :: [Text]
  , menuPlanDuration :: [Text]
  } deriving (Generic, Show)

instance FromMultipart Mem MenuPlanForm where
  fromMultipart multipartData =
    MenuPlanForm <$> lookupInputs "lat" multipartData
                 <*> lookupInputs "lng" multipartData
                 <*> lookupInputs "timezone" multipartData
                 <*> lookupInputs "datetime-start" multipartData
                 <*> lookupInputs "duration" multipartData

data MenuPinForm = MenuPinForm {
    menuPinFormTitle     :: Text
  , menuPinFormCategory  :: Text
  , menuPinFormLat       :: Text
  , menuPinFormLng       :: Text
  } deriving (Generic, Show)

instance FromMultipart Mem MenuPinForm where
  fromMultipart multipartData =
    MenuPinForm <$> lookupInput "title" multipartData
                <*> lookupInput "category" multipartData
                <*> lookupInput "lat" multipartData
                <*> lookupInput "lng" multipartData
