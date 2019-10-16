module Main where

import           Classifier
import           Control.Monad
import qualified Data.Bits                        as B
import qualified Data.Bits.Bitwise                as B
import           Data.Maybe
import           Data.Word
import           FlowGenerator
import qualified Numeric.Probability.Distribution as D
import           System.Random

main :: IO ()
main = do
    gen <- getStdGen -- use same generator so that the first elements are shared
    forM_ [2..15] $ printEdges gen

printEdges gen int = do
    let trainingset = take int . randomFlow $ gen
    let graphmodel = initGraphModel trainingset
    let edgesNow = edges graphmodel
    let numEdges = length edgesNow
    let edgeTuples = map edge edgesNow
    let igrs = map igr edgesNow
    putStrLn . show . filter ((==) 1.0 . snd) . zipWith (,) edgeTuples $ igrs
    putStrLn $ "number of edges: " ++ show numEdges
