module Data.Alphabet
  ( Alphabet(..)
  , AlphabetError(..)
  , AlphabetMonad
  , toAlphabet
  , toDefaultAlphabet
  , encode
  , decode
  , translate
  , alphabetRadix
  )
where

import           Control.Monad.Except
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
  deriving (Show)

-- | Error type for alphabet operations.
data AlphabetError a
  -- | Error returned when attempting to decode a letter which is not present
  -- in the current alphabet.
  = IllegalLetter a
  -- | Error returned when attempting to construct an alphabet for a radix 
  -- which is too large.
  | RadixTooLarge Int
  -- | Error returned when attempting to construct an alphabet for a radix
  -- which is too small.
  | RadixTooSmall Int

instance Show a => Show (AlphabetError a) where
  show (IllegalLetter l) = "illegal letter: " ++ show l
  show (RadixTooLarge r) = "radix " ++ show r ++ " is too large"
  show (RadixTooSmall r) = "radix " ++ show r ++ " is too small"

-- | Monad result type used for functions which may return an error.
--
-- Here, `a` is the letter type for the alphabet being wrapped. This is
-- required as `AlphabetError`, the error type being wrapped, must be
-- parameterized as such.
type AlphabetMonad a = Except (AlphabetError a)

-- | Creates an alphabet for a specific radix.
--
-- A function `f` is used to map cardinal values to symbols in the new alphabet.
-- This function must be a bijection defined on the range [0, radix).
--
-- This function is undefined for radices less than or equal to 1.
toAlphabet :: (Int -> a) -> Int -> Alphabet a
toAlphabet f radix | radix <= 1 = undefined
                   | otherwise  = Alphabet $ f <$> [0 .. maxDigit]
  where maxDigit = radix - 1

-- | Returns the default alphabet for a given radix.
--
-- The default alphabets are meant to be relatively natural for the specified
-- radix. For example, using the digits 0 through 7 for radix-8 or 0 through f 
-- for radix-16. For radix-26 through radix-52, the letters of the english 
-- alphabet are used instead of arabic numerals.
--
-- This function is only defined for radices in the range [2, 94]. For larger 
-- radices the `toAlphabet` function should be used instead.
toDefaultAlphabet :: Int -> AlphabetMonad Char (Alphabet Char)
toDefaultAlphabet radix
  | radix <= 1  = throwError $ RadixTooSmall radix
  | radix <= 10 = return $ toAlphabet (['0' .. '9'] !!) radix
  | radix <= 25 = return $ toAlphabet ((['0' .. '9'] ++ ['a' .. 'z']) !!) radix
  | radix <= 52 = return $ toAlphabet ((['a' .. 'z'] ++ ['A' .. 'Z']) !!) radix
  | radix <= 94 = return $ toAlphabet ((chr <$> [33 .. 126]) !!) radix
  | otherwise   = throwError $ RadixTooLarge radix

-- | Encodes a digit sequence in a given alphabet.
--
-- The digits will first be converted to the same radix as the alphabet before
-- encoding.
encode :: Eq a => Alphabet a -> Digits -> [a]
encode alpha ds = letterFromCardinal alpha <$> digits ds'
  where ds' = convertRadix (alphabetRadix alpha) ds

-- | The inverse operation of `encode`.
--
-- Unlike `encode` however, this function will throw an error if the string
-- being decoded is not written in the specified alphabet. For example, trying
-- to decode a hexadecimal string using the decimal alphabet.
decode :: Eq a => Alphabet a -> [a] -> AlphabetMonad a Digits
decode alpha src = do
  let cardinals = fmap (cardinalFromLetter alpha) src
  let r         = alphabetRadix alpha
  digitList <- sequence cardinals
  return $ Digits r digitList

-- | Translates a list of elements from one alphabet to another.
--
-- Returns an error if `input` contains elements which are not valid letters
-- in the source alphabet. For example, trying to translate decimal to hex but
-- `input` contains a 'a' character. 
translate :: Eq a => Alphabet a -> Alphabet a -> [a] -> AlphabetMonad a [a]
translate src dst input = encode dst <$> decode src input

-- | The radix corresponding to a given alphabet.
alphabetRadix :: Alphabet a -> Int
alphabetRadix = length . letters

-- | Converts a cardinal value into a letter.
--
-- It is assumed that the cardinal value does not exceed the radix of the
-- supplied alphabet.
letterFromCardinal :: Integral i => Alphabet a -> i -> a
letterFromCardinal alphabet = (letters alphabet !!) . fromIntegral

-- | Converts a letter into a cardinal value.
cardinalFromLetter :: Eq a => Alphabet a -> a -> AlphabetMonad a Integer
cardinalFromLetter alphabet letter =
  case elemIndex letter (letters alphabet) of
    Just index -> return $ toInteger index
    Nothing    -> throwError $ IllegalLetter letter
