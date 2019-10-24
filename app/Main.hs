{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Monad
import qualified Data.Bits                        as B
import qualified Data.Bits.Bitwise                as B
import qualified Data.ByteString.Lazy             as B
import qualified Data.ByteString.Lazy.Char8       as BC
import qualified Data.List                        as L
import           Data.Maybe
import           Data.Word
import qualified Numeric.Probability.Distribution as D
import           ProGraph.Graph
import           System.Environment
import           System.Exit
import qualified System.IO                        as IO
import           System.Random

main :: IO ()
main = do
    size <- (read . head) <$> getArgs :: IO Int
    filename <- last <$> getArgs
    handle <- IO.openFile filename IO.ReadMode
    ts <- tail <$> buildTrainingSet handle size []
    putStrLn $ show ts
    let initGraph = initGraphModel [head ts] size
    foldM_ (readAndClassify size) ([head ts],initGraph) $ tail ts

readAndClassify :: Int -> ([Flow],Graph) -> Flow -> IO ([Flow],Graph)
readAndClassify size (ts,graph) packet = do
    BC.putStrLn $ B.append "Processing packet:" $ B.pack packet
    let newts = packet : ts
    let newgraph = initGraphModel newts size
    let edgeTuples = map edge $ edges newgraph
    putStrLn $ show newgraph
    return (newts, newgraph)

buildTrainingSet handle size xs = do
    packet <- B.hGet handle size
    if (packet == B.empty)
        then return xs
        else buildTrainingSet handle size ((B.unpack packet):xs)
