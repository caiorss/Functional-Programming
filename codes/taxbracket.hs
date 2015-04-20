{-
---------------------------------------------------------------------------
                            TAX BRACKET CALCULATION
----------------------------------------------------------------------

Source: http://ayende.com/blog/108545/the-tax-calculation-challenge


People seems to be more interested in answering the question than the code that solved it. Actually, 
people seemed to be more interested in outdoing one another in creating answers to that. What I found most 
interesting is that a large percentage of the answers (both in the blog post and in the interviews) got a lot 
of that wrong.

So here is the question in full. The following table is the current tax rates in Israel:

                        Tax Rate
Up      to 5,070        10%
5,071   up to 8,660     14%
8,661   up to 14,070    23%
14,071  up to 21,240    30%
21,241  up to 40,230    33%
Higher  than 40,230     45%

Here are some example answers:

    5,000 –> 500
    5,800 –> 609.2
    9,000 –> 1087.8
    15,000 –> 2532.9
    50,000 –> 15,068.1

This problem is a bit tricky because the tax rate doesn’t apply to the whole sum, only to the part that is within the current rate.

------------------------------------------------------------------

http://gghez.com/c-net-implementation-of-a-progressive-tax-system/

A progressive tax system is a way to calculate a tax for a given price using brackets each taxed separately using its rate. The french tax on revenues is a good example of a progressive tax system.

            Bracket     Rate
0         – 10,000     10.50 %
10,000    – 45,000     25.60 %
           > 45,000    40.00 %

To calculate his taxation, John will have to do this calculation (see figure on left):

= (10,000 x 0.105) + (35,000 x 0.256) + (5,000 x 0.4)
 = 1,050 + 8,960 + 2,000
 = 12,010
 
 John will have to pay $ 12,010.

If John revenues was below some bracket definition (take $ 25,000 for example), only the last bracket containing the remaining amount to be taxed is applied :

= (10,000 x 0.105) + (15,000 x 0.256)

Here nothing is taxed in the last bracket range at rate 40.


*Main> map taxOfIsrael [5000, 5800, 9000, 15000, 50000]
[500.0,609.2,1087.8,2532.9,15068.1]
*Main> 


*Main> :r
[1 of 1] Compiling Main             ( taxbracket.hs, interpreted )
Ok, modules loaded: Main.
*Main> 
*Main> err
[0.0,0.0,0.0,0.0,0.0]
*Main> 
*Main> calc
calctax     calculated
*Main> calculated 
[500.0,609.2,1087.8,2532.9,15068.1]
*Main> 
*Main> expected 
[500.0,609.2,1087.8,2532.9,15068.1]
*Main> 
*Main> 

*Main> [1000, 2000.0, 9000.0, 10000.0, 15000.0, 20000.0, 30000.0, 50000.0, 60000.0, 100000.0, 1000000, 2000000] |>> taxrateOfIsrael |>> (*100) |>> round
[10,10,12,13,17,20,24,30,33,38,44,45]


-------------------------------------------------
-}


(|>) x f = f x
(|>>) x f = map f x
joinf functions element = map ($ element) functions

-- Infinite number
above = 1e30 

pairs xs = zip xs (tail xs)

progressivetax :: [[Double]] -> Double -> Double
progressivetax taxtable income = amount
            where 
            rates = taxtable |>> (!!1) |>> (/100.0)  |> tail
            levels = taxtable |>> (!!0)
            table = zip3 levels (tail levels) rates            
            amount = table |>> frow income |> takeWhile (>0) |> sum
            
            frow x (xlow, xhigh, rate) | x > xhigh = (xhigh-xlow)*rate 
                                       | otherwise = (x-xlow)*rate   



taxsearch taxtable value = result        
        where
        rows = takeWhile (\row -> fst row !! 0 <= value) (pairs taxtable)       
        result = case rows of 
                    [] -> taxtable !! 0
                    xs -> snd $ last rows

-- [(Salário Bruto – Dependentes – INSS) • Alíquota – Dedução] = IRRF
incometax taxtable income  = amount--(tax, aliquot, discount)
                where
                
                row = taxsearch taxtable income                
                aliquot = row !! 1
                discount = row !! 2                
                amount = income*(aliquot/100.0) - discount

taxrate taxfunction income = 100.0*(taxfunction income)/income

-- Progressive Tax System
israeltaxbrackets = [
    [0,          0],
    [ 5070.0, 10.0],
    [ 8660.0, 14.0],
    [14070.0, 23.0],
    [21240.0, 30.0],
    [40230.0, 33.0],
    [above  , 45.0]
    ]                    

-- http://www.revenuquebec.ca/en/citoyen/situation/nouvel-arrivant/regime-fiscal-du-quebec/taux-imposition.aspx
quebectaxbrackets = [
    [ 0.00000,  16.0],
    [ 41935.0,  16.0],
    [ 83650.0,  20.0],
    [ 102040.0, 24.0],
    [above  ,   25.25]
    ]                    




braziltaxbrackets = [
    [1787.77,    0,   0.00],
    [2679.29,  7.5, 134.48],
    [3572.43, 15.0, 335.03],
    [4463.81, 22.5, 602.96],
    [above,    27.5, 826.15]
   ]




taxOfIsrael = progressivetax israeltaxbrackets
taxrateOfIsrael = taxrate taxOfIsrael

--taxOfCountries = joinf [taxOfBrazil, taxOfIsrael]
--taxrateOfCountres income = taxOfCountries income |>> (\x -> 100*x/income)

taxOfBrazil = incometax braziltaxbrackets
taxrateOfBrazil = taxrate taxOfBrazil

taxOfquebec = progressivetax quebectaxbrackets
taxrateOfquebec = taxrate taxOfquebec

taxrateOfCountres = joinf [taxrateOfIsrael, taxrateOfBrazil, taxrateOfquebec]


tests       = [5000, 5800, 9000, 15000, 50000]
expected    = [500.0,609.2,1087.8,2532.9,15068.1]

calculated = map taxOfIsrael tests

err = (zipWith (-) expected calculated) |>> abs

