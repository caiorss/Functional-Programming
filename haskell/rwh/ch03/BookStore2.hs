
type CardHolder = String
type CardNumber = String
type Address = [String]
type CustomerID = Int

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)
