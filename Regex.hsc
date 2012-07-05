{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Regex where

import Data.ByteString (ByteString(..), useAsCString)
import Foreign hiding (unsafePerformIO)
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)

#include <pcre.h>

newtype PCREOption = PCREOption { unPCREOption :: CInt }
    deriving (Eq,Show)

-- caseless       :: PCREOption
-- caseless       = PCREOption #const PCRE_CASELESS

-- dollar_endonly :: PCREOption
-- dollar_endonly = PCREOption #const PCRE_DOLLAR_ENDONLY

-- dotall         :: PCREOption
-- dotall         = PCREOption #const PCRE_DOTALL

#{enum PCREOption, PCREOption
  , caseless             = PCRE_CASELESS
  , dollar_endonly       = PCRE_DOLLAR_ENDONLY
  , dotall               = PCRE_DOTALL
  }

combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0

type PCRE = ()

foreign import ccall unsafe "pcre.h pcre_compile"
    c_pcre_compile  :: CString
                    -> PCREOption
                    -> Ptr CString
                    -> Ptr CInt
                    -> Ptr Word8
                    -> IO (Ptr PCRE)

data Regex = Regex !(ForeignPtr PCRE)
                   !ByteString
        deriving (Eq, Ord, Show)

compile :: ByteString -> [PCREOption] -> Either String Regex
compile str flags = unsafePerformIO $
  useAsCString str $ \pattern -> do
    alloca $ \errptr       -> do
    alloca $ \erroffset    -> do
        pcre_ptr <- c_pcre_compile pattern (combineOptions flags) errptr erroffset nullPtr
        if pcre_ptr == nullPtr
            then do
                err <- peekCString =<< peek errptr
                return (Left err)
            else do
                reg <- newForeignPtr finalizerFree pcre_ptr -- release with free()
                return (Right (Regex reg str))
