{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Classifier
import           Control.Applicative
import           Control.Monad
import qualified Data.Bits                        as B
import qualified Data.Bits.Bitwise                as B
import qualified Data.ByteString.Lazy.Char8       as B
import qualified Data.List                        as L
import           Data.Maybe
import           Data.Word
import           FlowGenerator
import qualified Numeric.Probability.Distribution as D
import           System.Environment
import qualified System.IO                        as IO
import           System.Random

main :: IO ()
main = do
    size <- (read . head) <$> getArgs :: IO Int
    forever readAndClassify size

readAndClassify size = do
    packet <- B.hGet IO.stdin size
    B.putStrLn $ B.append "Processing packet:" packet

printEdges ts int = do
    let trainingset = take int ts
    let graphmodel = initGraphModel trainingset
    let edgesNow = edges graphmodel
    let edgeTuples = map edge edgesNow
    let igrs = map igr edgesNow
    return (edgeTuples, trainingset)

compareEdges a b = do
    let numEdges = length (fst b)
    putStrLn $ "Training set:" ++ show (snd b)
    putStrLn $ "Edges of graph:" ++ show (fst b)
    putStrLn $ "number of edges: " ++ show numEdges
    putStrLn $ "Edges removed from past iteration:" ++ (show $ (fst a) L.\\ (fst b))
    return b
