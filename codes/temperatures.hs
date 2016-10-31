-- From:http://rosettacode.org/wiki/Temperature_conversion#Haskell 
--
main = do
  putStrLn "Please enter temperature in kelvin: "
  input <- getLine
  let kelvin = read input :: Double
  if
    kelvin < 0.0
  then
    putStrLn "error"
  else
    let
      celsius = kelvin - 273.15
      fahrenheit = kelvin * 1.8 - 459.67
      rankine = kelvin * 1.8
    in do
      putStrLn ("kelvin: " ++ show kelvin)
      putStrLn ("celsius: " ++ show celsius)
      putStrLn ("fahrenheit: " ++ show fahrenheit)
      putStrLn ("rankine: " ++ show rankine)
