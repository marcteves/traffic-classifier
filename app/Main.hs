module Main where

import           Classifier
import           Control.Monad
import qualified Data.Bits                        as B
import qualified Data.Bits.Bitwise                as B
import           Data.Maybe
import qualified Data.List                        as L
import           Data.Word
import           FlowGenerator
import qualified Numeric.Probability.Distribution as D
import           System.Random

main :: IO ()
main = do
    gen <- getStdGen -- use same generator so that the first elements are shared
    let trainingset = randomFlow $ gen
    firstTuples <- printEdges trainingset 1
    result <- forM [2..15] $ printEdges trainingset
    foldM_ compareEdges firstTuples result

printEdges ts int = do
    let trainingset = take int ts
    let graphmodel = initGraphModel trainingset
    let edgesNow = edges graphmodel
    let edgeTuples = map edge edgesNow
    let igrs = map igr edgesNow
    return edgeTuples

compareEdges a b = do
    let numEdges = length b
    putStrLn $ "Edges of graph:" ++ show b
    putStrLn $ "number of edges: " ++ show numEdges
    putStrLn $ "Edges removed from past iteration:" ++ (show $ a L.\\ b)
    return b
