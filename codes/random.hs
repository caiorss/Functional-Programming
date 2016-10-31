-- https://www.fpcomplete.com/school/starting-with-haskell/libraries-and-frameworks/randoms
import System.Random
import Control.Monad (replicateM)

main = replicateM 10 (randomIO :: IO Float) >>= print
