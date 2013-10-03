{- |

Haskell implementation of a kalman filter

hmatrix requirements: sudo apt-get install libgsl0-dev liblapack-dev gnuplot imagemagic

-}
module Eddie.Kalman where

import Numeric.LinearAlgebra

data State = State { covariance :: Matrix Double, state :: Matrix Double } deriving (Eq, Show)

kalmanFilter :: Matrix Double      -- | state transition matrix 
                -> Matrix Double   -- | state projection matrix
                -> Matrix Double   -- | measurement noise
                -> State           -- | the old state
                -> Vector Double   -- | measurement of the observable state
                -> State           -- | the new state
kalmanFilter f h r s measurement = 
  let x = (state s) 
      p = (covariance s)
      -- prediction
      x' = f <> x -- + externalMotion TODO
      p' = f <> p <> (trans f)                 
      -- measurement update
      y  = (asColumn measurement) - h <> x' 
      s'  = h <> p' <> (trans h) + r
      k  = p' <> (trans h) <> (inv s')           -- kalman gain
      i  = ident $ rows p
      x'' = x' + k <> y
      p'' = (i - k<>h) <> p' 
  in State p'' x''
 
dt = 0.1 :: Double                          -- time step

-- initial covariance matrix
p0 =  (4><4) 
    [0, 0, 0,       0,
     0, 0, 0,       0,
     0, 0, 1000,    0,
     0, 0, 0,    1000] :: Matrix Double     -- initial uncertainty xy = 0 velocity 1000

-- state transition matrix for x y movement
f0 =  (4><4)
    [1, 0, dt, 0,
     0, 1, 0, dt,
     0, 0, 1,  0,
     0, 0, 0,  1] :: Matrix Double 

-- state projection matrix (x,y, vx, vy) -> (x,y). velocity is not directly observable
h0 =  (2><4)
    [1, 0, 0, 0,
     0, 1, 0, 0] :: Matrix Double  
                                   

-- noise / uncertainty of the measurement
r0 =  (2><2)
      [0.1,   0,
         0,  0.1] :: Matrix Double     

-- external motion
u0 = (4><1) [0,0,0,0] :: Matrix Double

example = kalmanFilter f0 h0 r0

-- example values from course
measurements = [[5, 10], [6, 8], [7, 6], [8, 4], [9, 2], [10, 0]]  :: [[Double]]

initialState = State p0 (asColumn $ fromList [4, 12, 0 ,0])

runExample :: [[Double]] -> State
runExample measurements = foldl (\s m -> example s (fromList m)) initialState measurements
