module Main where

import           Classifier
import qualified Data.Bits                        as B
import qualified Data.Bits.Bitwise                as B
import           Data.Maybe
import           Data.Word
import           FlowGenerator
import qualified Numeric.Probability.Distribution as D
import           System.Random

main :: IO ()
main = do
    gen <- getStdGen
    let trainingset = take 1000 . randomFlow $ gen
    putStrLn . show . initGraphModel $ trainingset
