
-- file: ch07/return3.hs
returnTest :: IO ()
returnTest =
  do one <- return 1
     let two = 2
     putStrLn $ show (one + two)

main :: IO ()  
main = returnTest 
