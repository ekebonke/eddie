{- |

Exercises for the udactity course "Artificial Intelligence for Robotics"

https://www.udacity.com/course/cs373 

-}
module Eddie.Localization where

import Prelude hiding (sum, map, foldl, foldl1, replicate, zipWith, length)
import Data.Vector

-- | uniform produces a uniform discrete distribution of the given size 
uniform :: Int -> Vector Double
uniform n = replicate n (1.0/(fromIntegral n))

normalize :: Vector Double -> Vector Double
normalize v = 
        let s = sum(v)
        in map (/ s) v

-- | a immutable one dimensional discrete world 
newtype ImmutableWorld a = ImmutableWorld { getWorld:: Vector a } 

-- | incorporates a new measurement into the curren belief (distribution)
sense :: ImmutableWorld z      -- ^ the immutable world
        -> (z -> z -> Double)  -- ^ maps the measurement to the prior probability 
        -> Vector Double       -- ^ the old belief state
        -> z                   -- ^ the measurment
        -> Vector Double       -- ^ the new belief state
sense world f distribution measurement = 
        let world' = map (f measurement) (getWorld world) -- map the measurement to a probability
            dist'  = zipWith (*) distribution world'      -- and multiply the current distribution
        in normalize dist'                                -- return the new normalized new distribution

moveExactCyclic :: Vector Double -> Int -> Vector Double
moveExactCyclic p u = 
        let len = length(p)
        in generate len (\i -> p ! ((len + (i-u `mod` len)) `mod` len))

moveCyclic ::Vector Double -> Vector Double -> Vector Double
moveCyclic p u = 
        let p' = map (*) 
            pExpanded = generate 5 (\i -> moveExactCyclic (map (* (p!i)) u) i)
        in foldl1 (zipWith (+)) pExpanded
