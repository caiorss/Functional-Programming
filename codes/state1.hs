-- State Monads testing
-- http://mugisha.me/category/Programming/entry/haskell_monads

import Control.Monad.State

harmonicStep ::  State  (Double,Double)   Double

harmonicStep = do
      (position,velocity) <- get
      let acceleration = (-0.01*position)
      let vel = velocity + acceleration
      let pos =  position + vel
      put (pos , vel)
      return position

harmonic :: State (Double,Double) [Double]
harmonic = do
     position <- harmonicStep
     laterPositions <- harmonic
     return (position:laterPositions)
