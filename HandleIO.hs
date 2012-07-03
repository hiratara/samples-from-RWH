{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HandleIO
    (
      HandleIO
    , Handle
    , IOMode(..)
    , runHandleIO
    , openFile
    , hClose
    , hPutStrLn
    ) where

import System.IO (Handle, IOMode(..))
import qualified System.IO as Sys
import Control.Monad.Trans (MonadIO(..))
import System.Directory (removeFile)

newtype HandleIO a = HandleIO { runHandleIO :: IO a }
    deriving (Monad, MonadIO)

openFile :: FilePath -> IOMode -> HandleIO Handle
openFile path mode = HandleIO $ Sys.openFile path mode

hClose :: Handle -> HandleIO ()
hClose = HandleIO . Sys.hClose

hPutStrLn :: Handle -> String -> HandleIO ()
hPutStrLn h s = HandleIO (Sys.hPutStrLn h s)

safeHello :: FilePath -> HandleIO ()
safeHello path = do
  h <- openFile path WriteMode
  hPutStrLn h "helloworld"
  hClose h

tidyHello :: FilePath -> HandleIO ()
tidyHello path = do
  safeHello path
  liftIO $ removeFile path

-- instance MonadIO HandleIO where
--     liftIO = HandleIO
