{-# LANGUAGE DuplicateRecordFields #-}

module OrderTaking.Domain where

import           Data.List.NonEmpty
import           OrderTaking.UnitQuantity

-- Product code related
newtype WidgetCode =
  WidgetCode String
  deriving (Show, Eq) -- constraint: starts with W then 4 digits

newtype GizmoCode =
  GizmoCode String
  deriving (Show, Eq)
  -- constraint: starts with G then 3 digits

data ProductCode
  = Widget WidgetCode
  | Gizmo GizmoCode
  deriving (Show, Eq)

-- Order Quantity related
newtype KilogramQuantity =
  KilogramQuantity Double
  deriving (Show, Eq)

data OrderQuantity
  = Unit UnitQuantity
  | Kilogram KilogramQuantity
  deriving (Show, Eq)

-- Order
data OrderId =
  OrderId
  deriving (Show, Eq)

data OrderLineId =
  OrderLineId
  deriving (Show, Eq)

data CustomerId =
  CustomerId
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

data Price =
  Price
  deriving (Show, Eq)

data BillingAmount =
  BillingAmount
  deriving (Show, Eq)

data ValidatedOrder = ValidatedOrder
  { orderId         :: OrderId
  , customerId      :: CustomerId
  , shippingAddress :: ValidatedAddress
  , billingAddress  :: BillingAddress
  , orderLines      :: NonEmpty OrderLine
  , amountToBill    :: BillingAmount
  } deriving (Show)

instance Eq ValidatedOrder where
  a == b = orderId (a :: ValidatedOrder) == orderId (b :: ValidatedOrder)

data OrderLine = OrderLine
  { orderLineId   :: OrderLineId
  , orderId       :: OrderId
  , productCode   :: ProductCode
  , orderQuantity :: OrderQuantity
  , price         :: Price
  } deriving (Show)

instance Eq OrderLine where
  a == b = orderLineId a == orderLineId b

data UnvalidatedOrder = UnvalidatedOrder
  { orderId         :: String
  , customerInfo    :: String
  , shippingAddress :: UnvalidatedAddress
  , billingAddress  :: String
  , orderLines      :: [String]
  , amountToBill    :: String
  } deriving (Show)

instance Eq UnvalidatedOrder where
  a == b = orderId (a :: UnvalidatedOrder) == orderId (b :: UnvalidatedOrder)

data UnvalidatedAddress =
  UnvalidatedAddress
  deriving (Show)

data ValidatedAddress =
  ValidatedAddress
  deriving (Show)

type AddressValidationService = UnvalidatedAddress -> Maybe ValidatedAddress

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

newtype PlaceOrderError =
  ValidationError [ValidationError]

data ValidationError = VError
  { fieldName        :: String
  , errorDescription :: String
  } deriving (Show, Eq)

--- The "Place Order" process
type PlaceOrder = UnvalidatedOrder -> Either PlaceOrderError PlaceOrderEvents
