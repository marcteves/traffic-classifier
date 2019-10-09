module Classifier where

import qualified Data.Bits                        as B
import qualified Data.Bits.Bitwise                as B
import qualified Data.List as L
import           Data.Word
import           Data.Maybe
import           FlowGenerator
import qualified Numeric.Probability.Distribution as D

type ProbDist = D.Distribution Float [Outcome]
type CondProbDist = D.Distribution Float ([Outcome], [Outcome])
type Outcome = Bool

data Node = Node
    { offsets :: [Int]
    , index :: Int
    , probDist :: ProbDist
    } deriving Show

data Edge = Edge
    { edge :: (Int, Int)
    , fstEntropy :: Float
    , sndEntropy :: Float
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
        probDist = D.empty :: ProbDist
    in  Node offsets index probDist

initGraphModel :: [Flow] -> Graph
initGraphModel trainingset =
    let outcomes = map B.toListBE trainingset
        numNodes = B.finiteBitSize . head $ trainingset
        initNodes = map initNode [1..numNodes]
        initGraph = newGraph initNodes []
        {- Sort the training set now so functions can below can assume this -}
        sortedTrainingSet = L.sort trainingset
    in graphModel sortedTrainingSet initGraph

-- iteratively refine graph model
graphModel :: [Flow] -> Graph -> Graph
graphModel trainingset graph =
    let newNodes = map (nodeProbDist trainingset) $ nodes graph
        nodePairs = filter ((2 == ) . length) $ L.subsequences $ nodes graph
        condProbDists = map (nodeCondProbDist trainingset) $ nodePairs
    in  Graph newNodes condProbDists

nodeProbDist :: [Flow] -> Node -> Node
nodeProbDist trainingset node =
    node { probDist = probDistFromFlows (offsets node) trainingset }

nodeCondProbDist :: [Flow] -> [Node] -> Edge
nodeCondProbDist trainingset nodes =
    let strip offsetList = map snd . filter (flip elem offsetList . fst) . zipWith (,) [1..]
        nodeA = head nodes
        nodeB = last nodes
        nodeAIndex = index nodeA
        nodeBIndex = index nodeB
        offsetsA = offsets nodeA
        offsetsB = offsets nodeB
        condProbDists = condProbDistFromFlows offsetsA offsetsB trainingset
    in Edge (nodeAIndex, nodeBIndex) 0 0 condProbDists

probDistFromFlows :: [Int] -> [Flow] -> ProbDist
probDistFromFlows offsetList trainingset =
    let indexedFlow = zipWith (,) [1..] . B.toListBE
        filteredFlow = map snd . filter (flip elem offsetList . fst)
        strippedTrainingSet = map (filteredFlow . indexedFlow) trainingset
        groupedTrainingSet = L.group strippedTrainingSet
    in  fromJust . D.normalize . D.fromList . zipWith (,) (map head groupedTrainingSet) $ (map (toEnum . length) groupedTrainingSet)

condProbDistFromFlows :: [Int] -> [Int] -> [Flow] -> CondProbDist
condProbDistFromFlows offsetA offsetB trainingset =
    let indexedFlow = zipWith (,) [1..] . B.toListBE
        filteredFlow o = map snd . filter (flip elem o. fst)
        strippedTrainingSetA = map (filteredFlow offsetA . indexedFlow) trainingset
        strippedTrainingSetB = map (filteredFlow offsetB . indexedFlow) trainingset
        groupedTrainingSet = L.group (zipWith (,) strippedTrainingSetA strippedTrainingSetB)
    in  fromJust . D.normalize . D.fromList . zipWith (,) (map head groupedTrainingSet) $ (map (toEnum . length) groupedTrainingSet)
