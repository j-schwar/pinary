module Data.Alphabet where

import           Data.Radix
import           Data.Char
import           Data.List
import           Data.Maybe

-- | A printable alphabet to encode data in.
--
-- Each letter in this alphabet must be unique. There is an implicit total
-- ordering enforced on the letters of an alphabet determined by their index
-- in this `letters` list. Elements with smaller indices are considered to be
-- semantically smaller than elements with larger indices.
--
-- For example, in the alphabet `['t', 'a', 'b']`, the letter 't' denotes the
-- semantic value 0, with 'a' being 1 and 'b' 2. The ordering of this alphabet
-- is as such: 't' < 'a' < 'b'.
newtype Alphabet a = Alphabet { letters :: [a] }

-- | Encodes a digit sequence in a given alphabet.
--
-- The digits will first be converted to the same radix as the alphabet before
-- encoding.
encode :: (Eq a, Ord a) => Alphabet a -> Digits -> [a]
encode alpha ds = (letters alpha !!) . fromIntegral <$> digits ds'
  where ds' = convertRadix (alphabetRadix alpha) ds

-- | The inverse operation of `encode`.
--
-- Unlike `encode` however, this function will throw an error if the string
-- being decoded is not written in the specified alphabet. For example, trying
-- to decode a hexadecimal string using the decimal alphabet.
--
-- For a safer version of this function, use `decodem`.
decode :: Eq a => Alphabet a -> [a] -> Digits
decode alpha src = Digits (alphabetRadix alpha) $ indexIn alpha <$> src
  where indexIn a = toInteger . fromJust <$> flip elemIndex (letters a)

-- | A safe version of `decode` which returns `Nothing` in the case where the
-- string is unable to be decoded using the specified alphabet.
decodem :: Eq a => Alphabet a -> [a] -> Maybe Digits
decodem alpha src = Digits (alphabetRadix alpha) . map toInteger <$> maybeDs
 where
  maybeDs = mapM (indexIn alpha) src
  indexIn a = flip elemIndex (letters a)

-- | The radix corresponding to a given alphabet.
alphabetRadix :: Alphabet a -> Int
alphabetRadix = length . letters

-- | The traditional binary (radix 2) alphabet.
binary :: Alphabet Char
binary = Alphabet ['0', '1']

-- | The traditional ternary (radix 3) alphabet.
ternary :: Alphabet Char
ternary = Alphabet ['0' .. '2']

-- | The traditional octal (radix 8) alphabet.
octal :: Alphabet Char
octal = Alphabet ['0' .. '7']

-- | The traditional decimal (radix 10) alphabet.
decimal :: Alphabet Char
decimal = Alphabet ['0' .. '9']

-- | The traditional hexadecimal (radix 16) alphabet.
hexadecimal :: Alphabet Char
hexadecimal = Alphabet $ letters decimal ++ ['a' .. 'f']

-- | The lowercase letters of the english alphabet as a radix 26 alphabet.
base26 :: Alphabet Char
base26 = Alphabet ['a' .. 'z']

-- | A combination of the lowercase and uppercase letters of the english
-- alphabet as a radix 52 alphabet.
--
-- The lowercase letters have a lower semantic value that the uppercase ones.
base52 :: Alphabet Char
base52 = Alphabet $ ['a' .. 'z'] ++ ['A' .. 'Z']

-- | A specialized alphabet utilizing all of the printable ASCII characters
-- (whitespace omitted). This alphabet has a radix of 94 since there are 94
-- unique printable ASCII characters.
base94 :: Alphabet Char
base94 = Alphabet $ chr <$> [33 .. 126]
