module Main where

import Control.Parallel.Strategies (NFData, rseq)
import Control.Monad (forM_)
import Data.List (foldl')
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S
import qualified Data.Map as M
import Text.Regex.PCRE.Light (compile, match)

import System.Environment (getArgs)
import LineChunks (chunkedReadWith)
import MapReduce (mapReduce)

countURLs :: [L.ByteString] -> M.Map S.ByteString Int
countURLs = mapReduce rseq (foldl' augment M.empty . L.lines)
                      rseq (M.unionsWith (+))
  where augment mapVal line =
            case match (compile pattern []) (strict line) [] of
              Just (_:url:_) -> M.insertWith' (+) url 1 mapVal
              _ -> mapVal
        strict  = S.concat . L.toChunks
        pattern = S.pack "\"(?:GET|POST|HEAD) ([^ ]+) HTTP/"

instance NFData S.ByteString -- Is it good?

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \path -> do
    numLines <- chunkedReadWith countURLs path
    putStrLn $ path ++ ": " ++ show numLines
