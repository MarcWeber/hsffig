--
-- This file contains a multiparameter class with functional dependencies
-- to be used by every module generated by HSFFIG. This class contains
-- combinators to access members of C structures. Every type corresponding
-- to a C structure type will be an instance of this class.
--

{-# OPTIONS -fglasgow-exts -ffi #-}

module HSFFIG.FieldAccess (
  FieldAccess (..)
) where

import Foreign
import Foreign.C.Types

class FieldAccess a b c | a c -> b where
  (==>) :: Ptr a -> c -> b
  (-->) :: Ptr a -> c -> IO b
  (<--) :: (Ptr a, c) -> b -> IO ()
  (==>) _ _ = error " illegal context for ==>"
  (-->) _ _ = error " illegal context for -->"
  (<--) _ _ = error " illegal context for <--"
