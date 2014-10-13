module Graph (adjacencyListGraph, breadthFirstSearch) where
import qualified Data.List as List
import qualified Data.IntSet as Set
import qualified Data.PriorityQueue.FingerTree as Queue
import Data.Array

type Node   = Int
type Weight = Int
type Edge   = (Node,Weight)
type Graph  = Node -> [Edge]
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

adjacencyListGraph :: Array Node [Edge] -> Graph
adjacencyListGraph edgesArray node = edgesArray ! node

neighbors :: Node -> Graph -> [Node]
neighbors node graph = map fst (graph node)

graphAlgorithm :: Queue Node -> Graph -> Node -> [Node]
graphAlgorithm queue graph node = walk (insert queue node) Set.empty [] where
    walk queue visited result
        | empty queue                 = result
        | Set.member node visited     = walk queueWithoutNode visited result
        | otherwise                   = walk queueWithNeighbors (Set.insert node visited) (node : result)
        where (node,queueWithoutNode) = extract queue
              queueWithNeighbors      = List.foldl' insert queue (neighbors node graph)

breadthFirstSearch :: Graph -> Node -> [Node]
breadthFirstSearch = graphAlgorithm stack

depthFirstSearch :: Graph -> Node -> [Node]
depthFirstSearch = undefined

dijkstra :: Graph -> Node -> [Node]
dijkstra = undefined
