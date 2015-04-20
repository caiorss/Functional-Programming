{-
Physical Engine Simulation

*Main> plotSpeed simuldata1
*Main> plotD
plotDispl  plotDots
*Main> plotDispl simuldata1
*Main> 
*Main> plotDispl simuldata2
*Main> plotSpeed simuldata2


-}
import Graphics.Gnuplot.Simple


type Time  = Double
type Displ = Double
type Speed = Double
type Accel = Double
type State = (Time, Displ, Speed)
type AccelFunction = State -> Accel


s2time :: State -> Displ
s2time (t, x, v) = t


s2speed :: State -> Speed
s2speed (t, x, v) = v

s2displ :: State -> Displ
s2displ (t, x, v) = x

--stateStep :: Time -> (State -> Accel) -> State -> State
stateStep :: Time ->  AccelFunction -> State -> State
stateStep dt a (t, x, v) =  (t', x', v')
          where
          t' = t + dt
          x' = x + v*dt
          v' = v + (a (t, x, v))*dt

--state_step = stateStep 0.1 accel

motion ::  Time -> AccelFunction -> (State -> Bool) -> State -> [State]
motion dt accelFunc constraintFunc start_state = states
    where
    state_generator = stateStep dt accelFunc
    states = takeWhile constraintFunc $ iterate  state_generator start_state

plotDispl :: [State] -> IO ()
plotDispl simuldata =  
    plotList [] ( zip  (map s2time simuldata ) (map s2displ simuldata))


plotSpeed :: [State] -> IO ()
plotSpeed simuldata =
    plotList [] ( zip  (map s2time simuldata ) (map s2speed simuldata))



--states = takeWhile (\s -> s2displ s >= 0) $ iterate  state_step (0, 0, 20)

until_time :: Time -> State -> Bool
until_time time state =  s2time state <= time         

-- Keeps the simulation running until the particle hits the ground
until_fall :: State -> Bool
until_fall state = s2displ state >= 0

{-
A rock is launched vertically from the ground x = 0 with the velocity of 20 m/s, 
regard the acceleration of gravity as 9.81, simulate the motion and 
generate the all data points until the rock is back to earch

-}

accel :: AccelFunction
accel (t, x, v) = -9.81 -- m/s2

simuldata1 = motion 0.1 accel until_fall (0.0, 0.0, 20.0)


{--
A pendulum with a 1 meter string is launched 
from 90 from the resting position with zero velocity.
Regard the gravity acceleration as 9.81 m/s2

--}
pendulumAccel :: Double -> State -> Accel
pendulumAccel rod_length (t, θ, dθ) = -9.81/rod_length*sin(θ)

-- Pendulum Acceleration for 1 meter lenght string
accpend =  pendulumAccel 1.0

simuldata2 = motion 0.01 accpend (until_time 3.0) (0, pi/2, 0)
angle_vector = map s2displ simuldata2
pendulum_xy = zip (map sin  angle_vector) (map (\θ -> 1-cos(θ)) angle_vector)


{-----------------------------------------------------------------------
Spring Mass Damping



You can see that the original 2nd-order ODE becomes 1st-order, which is called state-space method. One more thing, let's define the initial conditions so that the last thing is prepared for the coding part:
x0=1m
x0˙=1m/s
And the following parameters:

Mass                            m=50kg
Spring stiffness                k=5000N/m
Damping coefficient             c=100Ns/m

External sinusoidal force:      u(t)=500cos(5t)

The total time is 10s, and time step dt=0.001s

Now we have all resources prepared, then let's do some coding


http://nbviewer.ipython.org/github/Zhiweix/assignment-bank/blob/fb8a5e0aaec2af2fd263b38288738d88b1fa1091/Final%20projects/Final%20Project-Zhiwei%20Zhang/Spring-mass-damper%20system%20-%20Zhiwei%20Zhang.ipynb

-}


{-
    k - Spring stiffness N/m
    c - Damping coefficient
-}
accelSpring :: Double -> Double -> Double -> (State -> Double) -> State -> Accel
accelSpring m k c extForce  (t, x, v) = 
    (extForce (t, x, v))/m - k/m*x - c/m*v

-- Spring Acceleration as function of external force function
springAc_force =  accelSpring 50.0 5000.0 100.0 

force1 :: State -> Double
force1 (t, x, v) = 500*cos(5*t)

force2 :: State -> Double
force2 (t, x, v) =  100*t^2

force3 :: State -> Double
force3 (t, x, v) =  0.000

-- Attach Spring Mass-Damping system to force1
--springAccel = springAc_force force1

simulateSpring force = 
    motion 0.001 (springAc_force force) (until_time 10.0) (0.0, 1.0, 1.0)

simuldata3 =  simulateSpring force1
simuldata4 =  simulateSpring force2
simuldata5 =  simulateSpring force3

--plotSpeed = undefined

--plotDispl simulation 
