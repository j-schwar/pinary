{-# LANGUAGE TupleSections #-}

module Compression.Huffman
  ( Huffman
  , freqCompressor
  )
where

import           Compression
import           Control.Arrow
import           Data.Bit
import           Data.List
import           Data.Function
import qualified Data.Map.Strict               as Map

-- This module implements a simple, character-based, Huffman encoding algorithm
-- for strings.
--
-- Original Design: https://gist.github.com/kirelagin/3886243

-- | The Huffman compressor type.
--
-- Compresses/decompresses a string using Huffman encoding.
data Huffman a = Huffman { getCodeMap :: CodeMap a, getTree :: HTree }

instance Compressor Huffman where
  compress (Huffman cm _) = encode cm
  decompress (Huffman _ t) = decode t

-- | Constructs a Huffman compressor by performing a frequency analysis on the
-- characters in `str`.
--
-- The resultant compressor will perform an optimal compression on `str` but
-- will be limited to the character set which occurs in it.
freqCompressor :: Bit b => String -> Huffman b
freqCompressor str = Huffman cm tree
 where
  cm          = buildCodeMap tree
  tree        = buildTree frequencies
  frequencies = freqList str

-- | CodeMap is built from a Huffman tree and is used to encode data.
type CodeMap a = Map.Map Char [a]

-- | A Huffman tree is a simple binary tree where each leaf contains a character
-- and its weight.
--
-- Forks also have weights which are equivalent to the sum of their children.
data HTree
  = Leaf Char Int
  | Fork HTree HTree Int
  deriving (Show)

-- | The weight of a Huffman tree.
weight :: HTree -> Int
weight (Leaf _ w  ) = w
weight (Fork _ _ w) = w

-- | Merges two Huffman trees into a single one.
merge :: HTree -> HTree -> HTree
merge a b = Fork a b (weight a + weight b)

-- | Constructs a histogram mapping characters to the number of times they occur
-- in some string.
freqList :: String -> [(Char, Int)]
freqList = Map.toList . Map.fromListWith (+) . map (, 1)

-- | Constructs a Huffman tree from a list of character frequencies.
--
-- It sorts the list in ascending order by frequency, turning each (char, freq)
-- pair into a one-leaf tree, then continually merges the two smallest trees
-- together until there is only one tree remaining.
buildTree :: [(Char, Int)] -> HTree
buildTree = bld . map (uncurry Leaf) . sortBy (compare `on` snd)
 where
  bld [t         ] = t
  bld (a : b : cs) = bld $ insertBy (compare `on` weight) (merge a b) cs

-- | Constructs a Huffman tree from a string.
stringTree :: String -> HTree
stringTree = buildTree . freqList

-- | Traverses a Huffman tree to construct a code map for it.
buildCodeMap :: Bit a => HTree -> CodeMap a
buildCodeMap = Map.fromList . buildCodeList
 where
  buildCodeList (Leaf c _  ) = [(c, [])]
  buildCodeList (Fork l r _) = map (addBit zer) (buildCodeList l)
    ++ map (addBit one) (buildCodeList r)
    where addBit b = second (b :)

-- | Encodes a string using a given CodeMap.
encode :: Bit a => CodeMap a -> String -> [a]
encode m = concatMap (m Map.!)

-- | Encodes a string using a given Huffman tree.
encode' :: Bit a => HTree -> String -> [a]
encode' = encode . buildCodeMap

-- | Decodes a sequence of bits using a Huffman tree.
decode :: Bit a => HTree -> [a] -> String
decode tree = dcd tree
 where
  dcd (Leaf c _  ) []       = [c]
  dcd (Leaf c _  ) bs       = c : dcd tree bs
  dcd (Fork l r _) (b : bs) = dcd (if b == zer then l else r) bs
