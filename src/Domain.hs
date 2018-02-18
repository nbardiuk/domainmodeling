module Domain where

newtype CustomerId =
  CustomerId Integer
  deriving (Show, Eq)

newtype OrderId =
  OrderId Integer
  deriving (Show, Eq)

newtype GizmoCode =
  GizmoCode String
  deriving (Show, Eq)

newtype WidgetCode =
  WidgetCode String
  deriving (Show, Eq)

data ProductCode
  = Widget WidgetCode
  | Gizmo GizmoCode
  deriving (Show, Eq)

newtype UnitQuantity =
  UnitQuantity Integer
  deriving (Show, Eq)

newtype KilogramQuantity =
  KilogramQuantity Double
  deriving (Show, Eq)

data OrderQuantity
  = Unit UnitQuantity
  | Kilogram KilogramQuantity
  deriving (Show, Eq)

data CustomerInfo =
  CustomerInfo
  deriving (Show, Eq)

data ShippingAddress =
  ShippingAddress
  deriving (Show, Eq)

data BillingAddress =
  BillingAddress
  deriving (Show, Eq)

data OrderLine =
  OrderLine
  deriving (Show, Eq)

data AmountToBill =
  AmountToBill
  deriving (Show, Eq)

data Order = Order
  { customerInfo    :: CustomerInfo
  , shippingAddress :: ShippingAddress
  , billingAddress  :: BillingAddress
  , orderLines      :: [OrderLine]
  , amountToBill    :: AmountToBill
  } deriving (Show, Eq)

data UnvalidatedOrder =
  UnvalidatedOrder
  deriving (Show, Eq)

data ValidatedOrder =
  ValidatedOrder
  deriving (Show, Eq)

type ValidateOrder = UnvalidatedOrder -> ValidatedOrder

data AcknowledgmentSent =
  AcknowledgmentSent
  deriving (Show, Eq)

data OrderPlaced =
  OrderPlaced
  deriving (Show, Eq)

data BillableOrderPlaced =
  BillableOrderPlaced
  deriving (Show, Eq)

data PlaceOrderEvents = PlaceOrderEvents
  { acknowledgmentSent  :: AcknowledgmentSent
  , orderPlaced         :: OrderPlaced
  , billableOrderPlaced :: BillableOrderPlaced
  } deriving (Show, Eq)

type PlaceOrder = UnvalidatedOrder -> PlaceOrderEvents

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
