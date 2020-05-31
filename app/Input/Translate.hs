module Input.Translate where

import           Compression.Analysis
import           Data.Alphabet

-- | Returns the frequency table to use for compression.
compressionType :: String -> String -> Maybe [(Char, Int)]
compressionType "json"    _   = Just jsonFrequency
compressionType "ascii"   _   = Just asciiFrequency
compressionType "dynamic" src = Just $ analyseAsciiFrequency src
compressionType _         _   = Nothing
