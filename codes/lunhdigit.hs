{-
Luhn10 check digit algorithms 

    * http://www.ee.unb.ca/tervo/ee4253/luhn.shtml#apps
    * http://en.wikipedia.org/wiki/Luhn_algorithm

λ > luhn10check   "4563960122001999"
True
λ > 
λ > 

λ > luhn10check   "7992739871"
False
λ > 

λ > luhn10digit "7992739871"
"3"
λ >

λ > luhn10gen "7992739871"
"79927398713"
λ > 

λ > luhn10check $ luhn10gen  "7992739871"
True
λ > 

http://umairj.com/374/how-to-test-credit-card-numbers-using-luhns-algorithm/

This small function is very useful if you want to check the validity of the credit card number being provided before passing it to your payment mechanism.
Sample Credit Card Numbers for testing

When I implemented this in my code, I also wanted to have some sample credit card numbers to test this. Sample credit card numbers are also very useful in many cases so here are the credit card numbers that I found.
Credit Card Type    Credit Card Number
American Express    378282246310005
American Express    371449635398431
American Express Corporate  378734493671000
Australian BankCard     5610591081018250
Diners Club     30569309025904
Diners Club     38520000023237
Discover    6011111111111117
Discover    6011000990139424
JCB     3530111333300000
JCB     3566002020360505
MasterCard  5555555555554444
MasterCard  5105105105105100
Visa    4111111111111111
Visa    4012888888881881
Visa    4222222222222Note : Even though this number has a different character count than the other test numbers, it is the correct and functional number.
Processor-specific Cards
Dankort (PBS)   76009244561
Dankort (PBS)   5019717010103742
Switch/Solo (Paymentech)    6331101999990016

Original Source for these numbers is this.

I have just tried to sum up all the info that helped me with credit card numbers in one post. I hope this saves some time for other people.

-}


char2digit :: Char -> Integer
char2digit '0' = 0
char2digit '1' = 1
char2digit '2' = 2
char2digit '3' = 3
char2digit '4' = 4
char2digit '5' = 5
char2digit '6' = 6
char2digit '7' = 7
char2digit '8' = 8
char2digit '9' = 9


str2digits = map char2digit


-- Test case
--
--

number = "7992739871"




{- 

http://en.wikipedia.org/wiki/Luhn_algorithm

-}
luhn10digit number = show(digit)
    
    where
    digits = reverse $ str2digits number    
    -- Infinite sequence 2, 1, 2, 1 ...
    weights = map  (\n -> (mod n 2) + 1) [1..]
    sumdigits = sum $ map (\n -> mod n 10 + div n 10  ) $ zipWith (*) digits weights
    digit = 10 - (mod sumdigits 10)
    --digit = mod (9*sumdigits) 10

luhn10gen number = number ++ (luhn10digit number)

luhn10check number = digit == 0
    where
    digits = reverse $ str2digits number    
    -- Infinite sequence 1, 2, 1, 2, 1 ...
    weights = map  (\n -> (mod n 2) + 1) [0..]
    sumdigits = sum $ map (\n -> mod n 10 + div n 10  ) $ zipWith (*) digits weights
    digit = mod sumdigits 10


main = do
    code <- getLine
    print (luhn10gen code)

-- sample = [1..10] ?>> even |>> (+1)
