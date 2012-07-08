{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException(..), handle)
import qualified Data.ByteString.Lazy as L
import System.Console.Readline (readline)

import Codec.Compression.GZip (compress)

main :: IO ()
main = do
    maybeLine <- readline "Enter a file to compress> "
    case maybeLine of
      Nothing -> return ()
      Just "" -> return ()
      Just name -> do
           handle (\(e::SomeException) -> print $ show e) $ do
             content <- L.readFile name
             forkIO (compressFile name content)
--             compressFile name content
             return ()
           main
  where compressFile path = L.writeFile (path ++ ".gz") . compress
