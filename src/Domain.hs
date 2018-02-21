module Domain where

newtype EnvelopeContents =
  EnvelopeContents String
  deriving (Show, Eq)

data QuoteForm =
  QuoteForm
  deriving (Show, Eq)

data OrderForm =
  OrderForm
  deriving (Show, Eq)

data CategorizedMail
  = QuoteMail QuoteForm
  | OrderMail OrderForm
  deriving (Show, Eq)

type CategorizeInboundMail = EnvelopeContents -> CategorizedMail

newtype ContactId =
  ContactId Integer
  deriving (Show, Eq)

newtype PhoneNumber =
  PhoneNumber String
  deriving (Show, Eq)

newtype EmailAddress =
  EmailAddress String
  deriving (Show, Eq)

data Contact = Contact
  { contactId    :: ContactId
  , phoneNumber  :: PhoneNumber
  , emailAddress :: EmailAddress
  } deriving (Show)

instance Eq Contact where
  a == b = contactId a == contactId b
