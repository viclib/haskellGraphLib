{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Graph (adjacencyListGraph, breadthFirstSearch) where
import Prelude hiding (map)
import qualified Data.List as List
import qualified Data.IntSet as Set
import qualified Data.PriorityQueue.FingerTree as Queue
import Data.Vector (Vector,(!),foldl',map)

type Node   = Int
type Weight = Int
type Edge   = (Node,Weight)
type Graph  = Node -> Vector Edge

adjacencyListGraph :: Vector (Vector Edge) -> Graph
adjacencyListGraph edges node = edges ! (node - 1)

neighbors node graph = map fst (graph node)

data Queue element = Queue {
    insert  :: element -> Queue element,
    extract :: (element, Queue element),
    empty   :: Bool }

stack :: Queue a
stack = listToStack [] where
    listToStack container = Queue { 
        insert  = \node -> listToStack (node : container),
        extract = (head container, listToStack (tail container)),
        empty   = List.null container }

{-
-- This is faster, but I'm avoiding introducing typeclasses.
-- It would also make the intended implementation of dijkstra impossible.
class Queue container element | container -> element where
    new     :: container
    insert  :: container -> element -> container
    extract :: container -> (element, container)
    empty   :: container -> Bool

instance Queue [element] element where
    new     = []
    insert  = flip (:)
    extract = \ (x:xs) -> (x,xs)
    empty   = List.null

stack :: [Node]
stack = []
-}

graphAlgorithm queue graph node = walk (insert queue node) Set.empty [] where
    walk queue visited result
        | empty queue                 = result
        | Set.member node visited     = walk queueWithoutNode visited result
        | otherwise                   = walk queueWithNeighbors (Set.insert node visited) (node : result)
        where (node,queueWithoutNode) = extract queue
              queueWithNeighbors      = foldl' insert queue (neighbors node graph)

breadthFirstSearch :: Graph -> Node -> [Node]
breadthFirstSearch = graphAlgorithm stack

depthFirstSearch :: Graph -> Node -> [Node]
depthFirstSearch = undefined

dijkstra :: Graph -> Node -> [Node]
dijkstra = undefined
