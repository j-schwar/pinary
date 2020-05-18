{-# LANGUAGE PolyKinds #-}

module Compression where

import           Data.Bit

-- | The Compressor class provides two mutually inverse functions for converting
-- a string to, and from, a sequence of bits.
--
-- Implementors must enforce the invariant that `decompress c . compress c` 
-- is equivalent to the identity operation where `c` is the compressor itself.
class Compressor (c :: * -> *) where
  -- | Compresses a string into a sequence of bits.
  compress :: Bit b => c b -> String -> [b]
  -- | Decompresses a sequence of bits into a string.
  decompress :: Bit b => c b -> [b] -> String
