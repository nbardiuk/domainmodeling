module OrderTaking.UnitQuantity
  ( create
  , value
  , UnitQuantity
  ) where

newtype UnitQuantity =
  UnitQuantity Integer
  deriving (Show, Eq)

create :: Integer -> Either String UnitQuantity
create qty
  | qty < 1 = Left "Unit Quantity should be positive"
  | qty > 1000 = Left "Unit Quantity can not be more than 1000"
  | otherwise = Right (UnitQuantity qty)

value :: UnitQuantity -> Integer
value (UnitQuantity qty) = qty
