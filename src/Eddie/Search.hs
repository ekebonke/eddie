module Eddie.Search where

type Pred s = s -> Bool

data Node s a = 
   InitialNode { state :: s } |
   Node {
       parent :: Node s a,
       state :: s,
       action :: a,
       depth :: Int,
       cost :: Double } deriving (Show, Eq)

class Problem p where
    makeNode :: p -> Node s a -> a -> Node s a
    stepCost :: p -> Node s a -> a -> Double
    actions :: p -> s -> [a]


search :: (Problem p) => p -> s -> Pred s -> Maybe (Node s a)
search problem initialState goalTest = 
    Just $ InitialNode initialState
