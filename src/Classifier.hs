module Classifier where

import qualified Data.Bits                        as B
import qualified Data.Bits.Bitwise                as B
import qualified Data.List as L
import           Data.Word
import           FlowGenerator
import qualified Numeric.Probability.Distribution as D

type ProbDist = D.Distribution Float Outcomes
type CondProbDist = D.Distribution Float (Outcomes, Outcomes)
type Outcomes = [Bool]
data Node = Node
    { offsets :: [Int]
    , index :: Int
    , values :: [Bool]
    , probDist :: ProbDist
    } deriving Show

data Edge = Edge
    { edge :: (Int, Int)
    , condProbDist :: CondProbDist
    } deriving Show

data Graph = Graph
    { nodes :: [Node]
    , edges :: [Edge]
    } deriving Show

-- graphMethods
newGraph :: [Node] -> [Edge] -> Graph
newGraph nodes edges = Graph nodes edges

initNode :: Int -> Node
initNode int =
    let offsets = [int]
        index = int
        values = []
        probDist = D.empty :: ProbDist
    in  Node offsets index values probDist

initGraphModel :: [Flow] -> Graph
initGraphModel trainingset =
    let outcomes = map B.toListBE trainingset
        numNodes = B.finiteBitSize . head $ trainingset
        initNodes = map initNode [1..numNodes]
        initGraph = newGraph initNodes []
    in graphModel trainingset initGraph

-- iteratively refine graph model
graphModel :: [Flow] -> Graph -> Graph
graphModel trainingset graph =
    let newNodes = map (nodeProbDist trainingset) $ nodes graph
        nodePairs = filter ((2 == ) . length) $ L.subsequences $ nodes graph
        condProbDists = map (nodeCondProbDist trainingset) $ nodePairs
    in  Graph newNodes condProbDists

nodeProbDist :: [Flow] -> Node -> Node
nodeProbDist trainingset node =
    let strip offsetList = map snd . filter (flip elem offsetList . fst) . zipWith (,) [1..]
        probDists = foldr condInsert D.empty (map (strip (offsets node) . B.toListBE) trainingset)
    in  node { probDist = probDists }

nodeCondProbDist :: [Flow] -> [Node] -> Edge
nodeCondProbDist trainingset nodes =
    let strip offsetList = map snd . filter (flip elem offsetList . fst) . zipWith (,) [1..]
        nodeA = head nodes
        nodeB = last nodes
        nodeAIndex = index nodeA
        nodeBIndex = index nodeB
        valuesA = map (strip (offsets nodeA) . B.toListBE) trainingset
        valuesB = map (strip (offsets nodeB) . B.toListBE) trainingset
        condProbDists = foldr condProbInsert D.empty $ zipWith (,) valuesA valuesB
    in Edge (nodeAIndex, nodeBIndex) condProbDists

condInsert :: Outcomes -> ProbDist -> ProbDist
condInsert outcome distribution = D.insert (outcome, 1.0) distribution

condProbInsert :: (Outcomes, Outcomes) -> CondProbDist -> CondProbDist
condProbInsert outcome distribution = D.insert (outcome, 1.0) distribution
