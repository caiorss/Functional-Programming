--http://mitpress.mit.edu/sites/default/files/titles/content/sicm/book-Z-H-4.html#%_toc_start 

data Vec = Vec { xComp :: Double
                ,yComp :: Double
                ,zComp :: Double}

type Time           = Double
type Displacement   = Vec
type Velocity       = Vec
type State          = (Time, Displacement, Velocity)


type AccelerationFunction = State -> Vec

-- eulerStep :: AccelerationFunction -> Double -> State -> State
-- eulerStep a dt (t, r, v) = (t', r', v')
--     where
--         t' = t + dt
--         t' = r ^+^  v^*dt
--         v' = v ^+^ a(t, r, v) ^* dt
-- 
