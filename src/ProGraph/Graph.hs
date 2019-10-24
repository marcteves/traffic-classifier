module ProGraph.Graph where

import qualified Data.Bits                        as B
import qualified Data.Bits.Bitwise                as B
import qualified Data.List                        as L
import           Data.Maybe
import           Data.Word
import qualified Numeric.Probability.Distribution as D

type ProbDist = D.Distribution Float [Outcome]
type CondProbDist = D.Distribution Float ([Outcome], [Outcome])
type Outcome = Word8
type Flow = [Outcome]

-- A node can represent multiple adjacent bit/byte offsets
data Node = Node
    { offsets  :: [Int]
    , index    :: Int
    , probDist :: ProbDist
    , entropy  :: Float
    } deriving Show

data Edge = Edge
    { edge         :: (Int, Int)
    , condEntropy  :: Float
    , condProbDist :: CondProbDist
    , igr          :: Float
    } deriving Show

data Graph = Graph
    { nodes :: [Node]
    , edges :: [Edge]
    } deriving Show

-- graphMethods
newGraph :: [Node] -> [Edge] -> Graph
newGraph nodes edges = Graph nodes edges

getNodePair :: Graph -> Edge -> Maybe (Node, Node)
getNodePair g e = do
    let ns = nodes g
    nodeA <- L.find (matchIndex . fst . edge $ e) ns
    nodeB <- L.find (matchIndex . snd . edge $ e) ns
    return (nodeA, nodeB)
    where matchIndex i Node{index=ni} = i == ni

-- graph creation
initGraphModel :: [Flow] -> Int -> Graph
initGraphModel trainingset numNodes =
    let outcomes = trainingset
        initNodes = map (\int -> Node [int] int D.empty 0) [1..numNodes]
        initGraph = Graph initNodes []
        {- Sort the training set now so functions can below can assume this -}
        sortedTrainingSet = L.sort trainingset
    in graphModel sortedTrainingSet initGraph

-- iteratively refine graph model
graphModel :: [Flow] -> Graph -> Graph
graphModel trainingset graph =
    let newNodes = map (newNode trainingset) $ nodes graph
        nodePairs = filter ((2 == ) . length) $ L.subsequences $ newNodes
        allNodePairs =  nodePairs -- nodePairs ++ map reverse nodePairs
        edgeCandidates = map (edgeCandidate trainingset) $ allNodePairs
        strongEdges = filter ((==) 1.0 . igr) edgeCandidates -- use adaptive threshold next time
        mergedNodes = mergeNodes strongEdges newNodes
    in  Graph mergedNodes strongEdges

mergeNodes :: [Edge] -> [Node] -> [Node]
mergeNodes e n = n

newNode :: [Flow] -> Node -> Node
newNode ts n =
    let pd = probDistFromFlows (offsets n) ts
        entropy = computeNodeEntropy pd
    in n {
         probDist = pd
       , entropy = entropy
       }

nodeProbDist :: [Flow] -> Node -> Node
nodeProbDist trainingset node =
    node { probDist = probDistFromFlows (offsets node) trainingset }

computeNodeEntropy :: ProbDist -> Float
computeNodeEntropy pd =
    let probabilities = map snd $ D.toList pd
        information p = p * logBase 2 p
    in  negate $ foldr (+) 0 $ map information probabilities

edgeCandidate :: [Flow] -> [Node] -> Edge
edgeCandidate trainingset nodes =
    let nodeA = head nodes
        nodeB = last nodes
        nodeAIndex = index nodeA
        nodeBIndex = index nodeB
        offsetsA = offsets nodeA
        offsetsB = offsets nodeB
        condProbDist = condProbDistFromFlows offsetsA offsetsB trainingset
        condEntropy = computeCondEntropy (probDist nodeA) condProbDist
        igr = computeIGR (map entropy [nodeA, nodeB]) condEntropy
    in Edge (nodeAIndex, nodeBIndex) condEntropy condProbDist igr

computeIGR :: [Float] -> Float -> Float
computeIGR es ce =
    let minEntropy = minimum es
        xv = head es
        num = xv - ce
    in  if (abs minEntropy < 0.001) then 1.0 else
        if (abs num < 0.001) then 0 else
        num / minEntropy

computeCondEntropy :: ProbDist -> CondProbDist -> Float
computeCondEntropy pd cpd =
    let listpd = L.sortOn fst $ D.toList pd
        compareFirstOutcome a b = (fst . fst) a == (fst . fst) b
        listcpd = L.groupBy compareFirstOutcome $ L.sortOn (fst . fst) $ D.toList cpd
        information x xy = xy * logBase 2 (x / xy)
        pdpsonly = map snd listpd
        cpdpsonly = map (map snd) listcpd
    in  foldr (+) 0 $ concat $ zipWith (\x -> map (information x)) pdpsonly cpdpsonly

probDistFromFlows :: [Int] -> [Flow] -> ProbDist
probDistFromFlows offsetList trainingset =
    let strippedTrainingSet = map (stripFlow offsetList) trainingset
        groupedTrainingSet = L.group strippedTrainingSet
        headOfGroups = map head groupedTrainingSet
        lengthOfGroups = map (toEnum . length) groupedTrainingSet
    in  fromJust . D.normalize . D.fromList . zipWith (,) headOfGroups $ lengthOfGroups

condProbDistFromFlows :: [Int] -> [Int] -> [Flow] -> CondProbDist
condProbDistFromFlows offsetA offsetB trainingset =
    let strippedTrainingSetA = map (stripFlow offsetA) trainingset
        strippedTrainingSetB = map (stripFlow offsetB) trainingset
        groupedTrainingSet = L.group $ zipWith (,) strippedTrainingSetA strippedTrainingSetB
        headOfGroups = map head groupedTrainingSet
        lengthOfGroups = map (toEnum . length) groupedTrainingSet
    in  fromJust . D.normalize . D.fromList . zipWith (,) headOfGroups $ lengthOfGroups

stripFlow :: [Int] -> Flow -> Flow
stripFlow offsetList flow =
    let indexedFlow = zipWith (,) [1..]
        filteredFlow = map snd . filter (flip elem offsetList. fst)
    in  filteredFlow . indexedFlow $ flow
