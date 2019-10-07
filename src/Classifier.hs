module Classifier where

import qualified Data.Bits                        as B
import qualified Data.Bits.Bitwise                as B
import qualified Data.Graph                       as G
import           Data.Word
import           FlowGenerator
import qualified Numeric.Probability.Distribution as D

type ProbDist = D.Distribution Float Bool
type Outcomes = [Bool]

initGraphModel :: [Flow] -> G.Graph
initGraphModel trainingset =
    let outcomes = map B.toListBE trainingset
        numNodes = finiteBitSize . head $ trainingset
        initGraph = G.buildG (1, numNodes) []
    in graphModel trainingset initGraph

-- iteratively refine graph model
graphModel :: [Flow] -> G.Graph -> G.Graph
graphModel trainingset graph = id

updateProbDist :: [Outcomes] -> [ProbDist] -> [ProbDist]
updateProbDist trainingset distributions =
    let outcomes = map B.toListBE trainingset in
    foldr (zipWith condInsert) distributions outcomes

updateCondProbDist :: [Flow] -> ProbDist
updateCondProbDist = id

condInsert :: Outcome -> ProbDist -> ProbDist
condInsert outcome distribution = D.insert (outcome, 1.0) distribution
