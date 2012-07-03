{-# LANGUAGE MultiParamTypeClasses #-}

import MonadHandle
import qualified System.IO as Sys

import System.IO (IOMode(..))
import Control.Monad.Trans (MonadIO(..), MonadTrans(..))

import SafeHello

instance MonadHandle IO Sys.Handle where
    openFile = Sys.openFile
    hPutStr = Sys.hPutStr
    hClose = Sys.hClose
    hGetContents = Sys.hGetContents
    hPutStrLn = Sys.hPutStrLn
