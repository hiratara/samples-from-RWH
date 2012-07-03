{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances #-}

import qualified System.IO as Sys
import Control.Monad.Writer
import SafeHello
import MonadHandle

data Event = Open Sys.FilePath Sys.IOMode
           | Put String String
           | Close String
           | GetContents String
             deriving (Show)

newtype WriterIO a = W { runW :: Writer [Event] a }
    deriving (Monad, MonadWriter [Event])

runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO = runWriter . runW

instance MonadHandle WriterIO [Char] where
    openFile path mode = tell [Open path mode] >> return path
    hPutStr h str = tell [Put h str]
    hClose h = tell [Close h]
    hGetContents h = tell [GetContents h] >> return "fake contents"
