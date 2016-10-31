{-
Brazilian Finance Codes:

    * https://github.com/marcelorodrigo/rendafixa
    
https://www3.bcb.gov.br/sgspub/JSP/sgsgeral/FachadaWSSGS.wsdl


https://github.com/gugaime/jurus/blob/master/js/math/mathModule.js
https://github.com/gugaime/Spread/
https://github.com/gugaime/Spread/blob/master/QuantifyLib/BusinessDay.cs


http://monografias.poli.ufrj.br/monografias/monopoli10003147.pdf

-}


import Data.Time
import Data.Time.Clock.POSIX


(|>) x f = f x
(|>>) x f = map f x

pow x y = exp(y * log x)
geomean lst = pow (product lst) $ 1/(fromIntegral (length lst))
mean lst = sum lst / fromIntegral (length lst)
{- Standard Deviation-}
stdev values =  values   |>> (\x -> x -  mean values ) |>> (^2) |> mean |> sqrt
{- Standard Variance -}
stvar values = stdev values |> (^2)


date y m d = 
    UTCTime (fromGregorian y m d) (timeOfDayToTime $ TimeOfDay 0 0 0)

data Period =   Year
            |   Year360
            |   Year252
            |   Month
            |   Each3Months
            |   Each4Months
            |   Semester            
            |   Day252
            |   Day365
            |   Day360
            deriving (Eq, Show)

aa = Year
am = Month
as = Semester
at = Each3Months
aq = Each4Months
ad252 = Day252
ad365 = Day365
ad360 = Day360
du360 = Year360
du252 = Year252

period Year         = 1.0    -- Year
period Month        = 12.0   -- Month
period Semester     = 2.0    -- Semester
period Day252       = 252.0  -- Year 252
period Day360       = 360.0  -- 360 days years convention
period Day365       = 365.0  -- 365 adys years convention
period Each3Months  = 4.0    -- Each Year has 4 periods of 3 months




-- Write a nunmber a percent
pct x = x/100.0
-- Write a percent list
pctxs = map pct 

-- 
lagdiff lst = zipWith (-) (tail lst) lst
growth lst = zipWith (/) (lagdiff lst) lst

growth_p lst = map to_pct (growth lst)

-- Convert decimal to percent
to_pct x = 100.0*x
to_pctxs = map to_pct

-- Convert a percent number to decimal
from_pct x =  x / 100.0


--- Compound Future Value and Present Value
-- Future Value
fv i n pv = pv * pow (1+i) n
-- Present Value
pv i n fv = fv / pow (1 + i ) n

interest fv pv = fv - pv

-- Future Value Interest
ifv i n pv = (fv i n pv) - pv
-- Present Value Interest
ipv i n fv = fv - (pv i n fv)



-- Annual equivalent rate
anrate rate cap =  ( pow (1+ rate) ( period cap) ) -1
anrate_p rate cap = to_pct $ anrate (pct rate) cap

-- Equivalent Interest rate
eqvrate rate1 cap1 cap2 =
    (pow (1+rate1) (period cap1 / period cap2)) - 1

eqvrate_p rate1 cap1 cap2 =
    to_pct $ eqvrate (pct rate1) cap1 cap2

efrate rate cap  =
    (pow (1 + rate / (period cap)) (period cap) ) -  1

-- http://www.investopedia.com/terms/e/effectiveinterest.asp    
efrate_pct rate cap = to_pct $ efrate (pct rate) cap


-- Equivalent Interest raate
totalrate rates = (foldr (*) 1 $ map (1.0+) rates) - 1

totalrate_p rates = to_pct $ totalrate (map pct rates)

-- http://guifleury.com/blogengine/page/DERIVATIVOS
--
--
-- http://linkconcursos.com.br/o-que-significa-taxa-over-definicao-e-caracteristicas/
-- http://www.bertolo.pro.br/AdminFin/HTML/OVER.htm
-- http://www2.bmf.com.br/cimConteudo/W_ArtigosPeriodicos/conjuntura173.pdf
rate1bu rate = (pow (1 + rate) (1.0/252.0)) - 1
rate1bu_p rate = to_pct $ rate1bu (pct rate)

buTd360 rate = 
    pow ( 1 + rate) (360.0/252.0) -1

d360Tbu rate = 
    pow ( 1 + rate) (252.0/360.0) -1
