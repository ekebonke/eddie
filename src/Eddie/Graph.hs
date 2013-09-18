module Eddie.Graph where

import qualified Data.Map as M

data Vertex v = Vertex { value :: v } deriving (Show, Eq, Ord)
data Edge v l = Edge { from :: Vertex v, to :: Vertex v, label :: l } deriving (Show,Eq)

data Graph v l = Graph {
        incoming :: M.Map (Vertex v) [Edge v l],
        outgoing :: M.Map (Vertex v) [Edge v l]
} deriving (Show)

connect :: (Ord v) => Graph v l -> Vertex v -> Vertex v -> l -> Graph v l
connect graph v1 v2 l = 
        let e = Edge v1 v2 l 
        in Graph (M.insertWith (++) v2 [e] (incoming graph)) (M.insertWith (++) v1 [e] (outgoing graph))

newGraph = Graph M.empty M.empty


outEdges :: (Ord v) => Graph v l -> Vertex v -> [Edge v l] 
outEdges (Graph _ out) vertex = case M.lookup vertex out of  
  Nothing -> []
  Just a  -> a

