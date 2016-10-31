{- 
Demo Application Showing how to convert JSON 

-}
import Network.HTTP
import Data.Text
import GHC.Generics

import Text.JSON
import Text.JSON.Generic


url_USD2BRL = "http://rate-exchange.appspot.com/currency?from=USD&to=BRL"

geturl url = simpleHTTP (getRequest url) >>= getResponseBody

data Conversion =
  Conversion { to :: !Text
             , rate :: Double
             , from :: !Text
             , v :: Double
               } deriving (Eq, Show, Data, Typeable)



