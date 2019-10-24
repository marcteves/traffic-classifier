module ProGraph.Classifier where

import qualified Data.Bits                        as B
import qualified Data.Bits.Bitwise                as B
import qualified Data.List                        as L
import           Data.Maybe
import           Data.Word
import qualified Numeric.Probability.Distribution as D
import           ProGraph.Graph

classify :: Graph -> Flow -> Bool
classify g@(Graph nodes edges) packet =
    let allNodes = checkAllNodes nodes packet
        allEdges = checkAllEdges g edges packet
    in True && allEdges

checkAllNodes :: [Node] -> Flow -> Bool
checkAllNodes (n:ns) packet =
    let offsetList = offsets n
        valids = map fst . D.toList . probDist $ n :: [Flow]
        strippedPacket = stripFlow offsetList packet :: Flow
        nodesMatched = foldr (matchNode strippedPacket) True $ valids
    in if nodesMatched == False then False else checkAllNodes ns packet
    where matchNode s v prev = if prev == False then False else v == s
checkAllNodes [] packet = True

checkAllEdges g (e:es) packet =
    let (nodeA, nodeB) = fromJust . getNodePair g $ e
        oslA = offsets nodeA
        oslB = offsets nodeB
        stripA = stripFlow oslA packet
        stripB = stripFlow oslB packet
        strippedPacket = (stripA, stripB)
        valids = map fst . D.toList . condProbDist $ e :: [(Flow, Flow)]
        edgesMatched = foldr (matchEdge strippedPacket) True $ valids
    in if edgesMatched == False then False else checkAllEdges g es packet
    where matchEdge s v prev = if prev == False then False else v == s

checkAllEdges g [] packet     = True
