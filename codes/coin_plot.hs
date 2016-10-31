import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import System.Random

trial frac = scanl (*) 1 (map f bits)
  where
    b = 0.1

    f True = (1+frac*(1+b))
    f False = (1-frac)
    bits = randoms $ mkStdGen 0

vals :: Double -> [ (Double,LogValue) ]
vals frac = [(fromIntegral x, LogValue y) | (x,y) <- filter (\(x,_)-> x `mod` (m+1)==0) $ take n $ zip [0..] (trial frac)]
  where
    n = 1001
    m = 0

main = toFile def "example4_big.png" $ do
    layout_title .= "Simulation of betting on a biased coin"
    plot (line "f=0.05" [vals 0.05 ])
    plot (line "f=0.1" [vals 0.1])
