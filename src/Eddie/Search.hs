{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances, ViewPatterns #-}
module Eddie.Search where

import qualified Data.Sequence as S
import qualified Data.Set as Set
import Data.Maybe (isJust)

type Pred s = s -> Bool

data Node s a = 
   Node {
       parent :: Maybe (Node s a),
       state :: s,
       action :: a,
       depth :: Int,
       cost :: Double } deriving (Show, Eq, Ord)

class Problem p s a | p -> s a where
    stepCost :: p -> Node s a -> a -> Double
    actions :: p -> s -> [a]
    expand :: p -> s -> a -> s

class Frontier f s a | f -> s a where
    add :: f -> Node s a -> f
    get :: f -> (f, Maybe (Node s a))
    member :: f -> Node s a -> Bool
    addAll :: f -> [Node s a] -> f
    addAll f [] = f
    addAll f (x:xs) = addAll (add f x) xs

instance (Eq s, Eq a) => Frontier (S.Seq (Node s a)) s a where
   add seq' node = seq' S.|> node
   get (S.viewl -> S.EmptyL) = (S.empty, Nothing)
   get (S.viewl -> e S.:< rest) = (rest, Just e)
   member seq' e  = if S.null seq' then False else isJust $ S.findIndexL (== e) seq'

makeNode :: (Problem p s a, Ord s, Eq s) => p -> Node s a -> a -> s -> Node s a
makeNode problem p@(Node parent state action depth cost) currentAction newState = 
    Node (Just p) newState currentAction (1 + depth) (cost + stepCost problem p currentAction)

search :: (Problem p s a, Ord s, Eq s, Ord a, Eq a) => p -> Node s a -> Pred s -> Maybe (Node s a)
search problem initialNode goalTest = 
  if goalTest (state initialNode)
  then Just initialNode
  else let frontier    = add (S.empty:: S.Seq (Node s a)) initialNode 
           explored    = Set.empty
       in loop problem frontier explored goalTest

loop :: (Problem p s a, Frontier f s a, Ord s) => p -> f -> Set.Set s -> Pred s -> Maybe (Node s a) 
loop problem frontier explored goalTest =
  case get frontier of
    (_,    Nothing)                       -> Nothing
    (rest, Just n@(Node _  state' _ _ _)) -> 
      let state'     = state n
          actions'   = actions problem state'
          newNodes   = map (\a -> makeNode problem n a $ expand problem state' a) actions'
          newNodes'  = filter (\n -> not $ Set.member (state n) explored) newNodes
          newNodes'' = filter (\n -> not $ member frontier n) newNodes'
          goalNodes  = filter (\n -> goalTest (state n)) newNodes''
      in case goalNodes of 
        []   -> loop problem (frontier) explored goalTest
        x:xs -> Just x
