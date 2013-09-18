{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Eddie.Examples where

import Data.List (find)
import Data.Maybe (fromJust)
import Eddie.Graph (newGraph, Vertex(..), connect, to, Graph(..), outEdges, label)
import qualified Eddie.Search as S
           
cities = [
  ("Berlin", "Wien",      1.3),
  ("Berlin", "Prag",      0.7),
  ("Berlin", "Paris",     2.2),
  ("Prag",   "Wien",      0.7),
  ("Wien",   "Paris",     3.2),
  ("Paris",  "Rom",       2.8),
  ("Wien",   "Rom",       1.5),
  ("Rom",    "Madrid",    2.1),
  ("Paris",  "Madrid",    2.0),
  ("Prag",   "Budapest",  0.9)]

cityGraph = 
    let g = newGraph
        f = \g t -> let (v1,v2,l) = t in  connect g (Vertex v1) (Vertex v2) l
        in foldl f g cities
           
instance S.Problem (Graph [Char] Double) [Char] [Char] where
    stepCost graph n action = fromJust $ fmap (label) edge -- using fromJust is not so good 
                              where edge = Data.List.find (\e -> (value (to e)) == action) $ outEdges graph (Vertex (S.state n))
    actions graph s = map (value . to) $ outEdges graph (Vertex s) 
    expand graph s action = action
    
    
