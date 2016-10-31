{-
   http://www.xe.com/symbols.php 
  
   http://www.fatvat.co.uk/2010/07/foreign-exchange-arbitrage.html 


    > showExchange (againstBRL GBP)
    "GBP/BRL =  0.22624423496905544"
    > 
    > showExchange (againstUSD GBP)
    "GBP/USD =  0.648511"
    > 
    > 


    
-}
import Text.Printf


-- Pipe Operators
(|>) x f = f x
(|>>) x f = map f x
(?>>) x f = filter f x

 
enumerate list = zip [0..(length list)]  list

-- findElement list element  = 


data Currency = USD | EUR | GBP | AUD | CAD | JPY | CNY |  KWR | SGD | BRL | INR
                deriving(Eq, Enum, Show, Read, Ord)
--                  
--                CurrencyX , Currency-Base  --> Exchange Rate
--                CurrencyX/CurrencyBase
type CurencyPair = (Currency, Currency)

data ExchangeRate = ExchangeRate {
    currencyPair  :: CurencyPair,
    exchangeRate  :: Double

} deriving(Eq, Show)


-- Exchange Tables Against USD
-- Prices of currencies, or exchange rates in USD $ 
-- in the day 19/21/2014
--
ratetable = [
    (USD,  1.000000),
    (EUR,  0.880281),
    (GBP,  0.648511),
    (CAD,  1.24904 ),
    (AUD,  1.28299 ),
    (JPY,  119.057 ),
    (BRL,  2.86642 ),
    (KWR,  1112.37 ),
    (INR,  62.187  )
    ]          




-- Find the position of the first element in the list
-- findElement :: Eq a => [a] -> a -> Maybe (Int, a) 

--  Find Element in a list and return the position of first element found
--
--
findElement :: Eq b => [b] -> b -> Maybe Int
findElement list elem = case result of
                        []  -> Nothing
                        [x] -> Just(fst x)
                        xs  -> Just(fst $ xs !! 0)
                        where
                        result = list |> enumerate |> filter (\el -> snd el == elem ) 

fromJust :: Maybe t -> t
fromJust (Just x) = x
--fromJust Nothing  = Nothing


--
--  table   - table of rows ( tuples)
--  column  - A column that will be looked up 
--  elemn   - Element to be searched in the column
--
--  :return: The row that has the searched element
--
lookupTable :: Eq b => [a] -> [b] -> b -> Maybe a
lookupTable table column elem =  case idx of 
                            Nothing -> Nothing
                            Just n  -> Just(table !! n)
                            where 
                            idx = findElement column elem

--getExchangeRate :: (Currency a, Currency b, ExchangeRate c)=>  (a, b) -> c

lookupt = lookupTable ratetable (map fst ratetable)


getExchangeRate currency1 currency2 = 
                    ExchangeRate { currencyPair = (currency1, currency2), exchangeRate = rate   }
                    where                      
                    row1 = lookupt currency1 |> fromJust
                    row2 = lookupt currency2 |> fromJust
                    
                    rate = (snd row2)/(snd row1)

-- List of all available currencies
currencies = map fst ratetable

-- Calculates the exchange Rate against this currency
againstBRL = getExchangeRate BRL 
againstUSD = getExchangeRate USD
againstGBP = getExchangeRate GBP

-- List of all exchange rates aginst USD and BRL
exchangesAgainstUSD = map againstUSD currencies
exchangesAgainstBRL = map againstBRL currencies

--showExchange :: ExchangeRate -> IO ()
showExchange exchange = show(currency2) ++ "/" ++ show(currency1) ++ " =  " ++ show(rate)
                      where
                      currency1 = fst $ currencyPair exchange
                      currency2 = snd $ currencyPair exchange
                      rate      =       exchangeRate exchange

                       
-- getExchangeRate Curr1  Curr2 = 
