{- |

Exercises for the udactity course "Artificial Intelligence for Robotics"

https://www.udacity.com/course/cs373 

-}
module Eddie.Localization where

import Prelude hiding (sum, map, foldl, foldl1, replicate, zipWith, length, zip)
import Data.Vector

-- | uniform produces a uniform discrete distribution of the given size 
uniform :: Int -> Vector Double
uniform n = replicate n (1.0/fromIntegral n)

normalize :: Vector Double -> Vector Double
normalize v = 
        let s = sum v
        in map (/ s) v

-- | a immutable one dimensional discrete world 
newtype ImmutableWorld a = ImmutableWorld { getWorld:: Vector a } 

-- | incorporates a new measurement into the current belief (distribution) this implementation uses bayes rule
sense :: ImmutableWorld z      -- ^ the immutable world
        -> (z -> z -> Double)  -- ^ maps the measurement to the measurement probability 
        -> Vector Double       -- ^ the old belief state, aka 'prior'
        -> z                   -- ^ the measurement
        -> Vector Double       -- ^ the new belief state, aka 'posterior'
sense world f distribution measurement = 
        let world' = map (f measurement) (getWorld world) -- map the actual measurement to the measurement probability
            dist'  = zipWith (*) distribution world'      -- and multiply the prior with the measurement prob, yielding the non normalized posterior distribution
        in normalize dist'                                -- return the new normalized new distribution

moveExactCyclic :: Vector Double -> Int -> Vector Double
moveExactCyclic p u = 
        let len = length p
        in generate len (\i -> p ! ((len + (i-u `mod` len)) `mod` len))

moveCyclic :: Vector Double   -- ^ initial belief state
        -> Vector Double      -- ^ relative distribution for the move 
        -> Vector Double
moveCyclic p u = 
        let f i = map (* (p!i)) u  -- mulitply the state probabilty with the move probability
                                   -- this produces a vector of vectors i.e a "matrix" of result
            p' = generate 5 (\i -> moveExactCyclic (f i) i) -- and shift the result to the correct position
        in foldl1 (zipWith (+)) p' -- sum the vectors for the posterior distribution (convolution) - total probability

-- | intended to be curried with a and b as free parameters
hitMissSense:: Eq z => Double -> Double -> (z -> z -> Double)
hitMissSense pHit pMiss a b = if a == b then pHit else pMiss

run:: ImmutableWorld z 
        -> (z -> z -> Double)     -- hit / miss probability
        -> Vector Double          -- initial belief
        -> Vector (Vector Double) -- motions
        -> Vector z               -- measurements
        -> Vector Double          -- final belief
run world f p motions measurements = 
        let sense' = sense world f
            steps  = zip measurements motions
            act q (measurement, motion) = moveCyclic (sense' q measurement) motion
        in foldl act p steps

runSimple:: (Eq z) => ImmutableWorld z -> Vector Double -> Vector z -> Vector Double
runSimple world motion measurements = 
        let worldSize = length $ getWorld world
            f         = hitMissSense 0.6 0.2
            initialBelief = uniform worldSize
            motions       = replicate (length measurements) motion  -- same motion forward
        in run world f initialBelief motions measurements