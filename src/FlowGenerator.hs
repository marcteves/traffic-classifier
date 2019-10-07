module FlowGenerator
    ( randomFlow
    , Request
    , Response
    , Flow
    ) where

import           Data.Word
import           System.Random

-- type synonyms to make the type signature clearer
type Request = Word8 -- unused for now
type Response = Word8 -- unused for now
type Flow = Word16 -- merged RequestResponse

-- infinite list of request response tuples
randomFlow :: RandomGen g => g -> [Flow]
randomFlow gen = do
    request <- randomRs (0x61, 0x7A) gen -- all requests are lowercase ASCII
    response <- return (request - 0x20) -- responses are uppercase equivalents
    return $ request * 0x100 + response
