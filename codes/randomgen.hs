
import System.Random
import Control.Monad (replicateM)

flipCoin :: IO Integer
flipCoin = randomRIO (0, 1)

flipCoinNtimes n = replicateM n flipCoin

frequency elem alist = length $ filter (==elem) alist

relativeFreq :: Integer -> [Integer] -> Double
relativeFreq elem alist = 
    fromIntegral (frequency elem alist) / fromIntegral (length alist)

simulateCoinToss :: Int -> IO ([Double], [Int])
simulateCoinToss ntimes =  do
    serie <- (flipCoinNtimes  ntimes)
    let counts = map (flip frequency serie)   [0, 1]
    let freqs = map (flip relativeFreq serie) [0, 1]
    return (freqs, counts)


    
showSimulation ntimes = do
    result <- simulateCoinToss ntimes
    let p_tails = (fst result) !! 0
    let p_heads = (fst result) !! 1
    
    let n_tails = (snd result) !! 0
    let n_heads = (snd result) !! 1
    
    let tosses = n_tails + n_heads
    let p_error = abs(p_tails - p_heads)
    
    putStrLn $ "Number of tosses : " ++ show(tosses)
    putStrLn $ "The number of tails is : " ++ show(n_tails)        
    putStrLn $ "The number of heads is : " ++ show(n_heads)
    putStrLn $ "The % of tails is : " ++ show(100.0*p_tails)
    putStrLn $ "The % of heads is :" ++ show(100.0*p_heads)
    putStrLn $ "The %erro is : "  ++ show(100*p_error)
    putStrLn "\n-------------------------------------"
    
