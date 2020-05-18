module Input.Translate where

import           Compression.Analysis
import           Data.Alphabet

-- | Returns the character-alphabet associated with a name.
alphabetName :: String -> Maybe (Alphabet Char)
alphabetName "binary"      = Just binary
alphabetName "base2"       = Just binary
alphabetName "ternary"     = Just ternary
alphabetName "base3"       = Just binary
alphabetName "octal"       = Just octal
alphabetName "base8"       = Just octal
alphabetName "decimal"     = Just decimal
alphabetName "base10"      = Just decimal
alphabetName "hexadecimal" = Just hexadecimal
alphabetName "hex"         = Just hexadecimal
alphabetName "base16"      = Just hexadecimal
alphabetName "base26"      = Just base26
alphabetName "base52"      = Just base52
alphabetName "base94"      = Just base94
alphabetName _             = Nothing

-- | Returns the frequency table to use for compression.
compressionType :: String -> String -> Maybe [(Char, Int)]
compressionType "json"    _   = Just jsonFrequency
compressionType "ascii"   _   = Just asciiFrequency
compressionType "dynamic" src = Just $ analyseAsciiFrequency src
compressionType _         _   = Nothing
