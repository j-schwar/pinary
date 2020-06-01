module Encode where

import           Control.Monad.Except
import qualified Data.Alphabet                 as A
import           Data.ByteString                ( ByteString )
import           Data.Error
import           Data.List                      ( intercalate )
import           Data.List.Split                ( splitOn )
import qualified Data.Radix                    as Radix
import           Data.Typeable

-- | Class for types which can be encoded an decoded using an Alphabet.
class Encode e where
  -- | A digit representation of this type.
  asDigits :: e -> Radix.Digits

  -- | Constructs this type from a digit representation.
  fromDigits :: Radix.Digits -> Except Error e

  -- | Encodes a value by converting it to some digits then encoding those
  -- digits using an alphabet.
  encode :: Eq a => A.Alphabet a -> e -> [a]
  encode alphabet = A.encode alphabet . asDigits

  -- | Decodes a list of letters by converting them to digits using an
  -- alphabet then converting those digits into a value.
  decode :: (Eq a, Show a, Typeable a) => A.Alphabet a -> [a] -> Except Error e
  decode alphabet src = do
    digits <- generalizeExceptT $ A.decode alphabet src
    fromDigits digits

instance Encode Integer where
  asDigits   = Radix.fromBase10
  fromDigits = return . Radix.toBase10

instance Encode Int where
  asDigits   = asDigits . toInteger
  fromDigits = return . fromIntegral . Radix.toBase10

instance Encode ByteString where
  asDigits   = Radix.fromByteString
  fromDigits = return . Radix.toByteString

-- | Error type thrown when failed to split an encoded string into a 
-- specific number of values.
data DecodeError a = FailedToSplit a Int Int

instance Show a => Show (DecodeError a) where
  show (FailedToSplit letter expected actual) =
    "failed to split with letter: "
      ++ show letter
      ++ ", expected "
      ++ show expected
      ++ " values, got "
      ++ show actual

instance (Show a, Typeable a) => Exception (DecodeError a)

-- | Encodes a sequence of values into a list of letters in a specific alphabet
-- where each value is separated by a specific letter.
--
-- While `sep` (the letter used to separate values) must be of the same type as
-- the letters in alphabet being used to encode the values, it should be in the
-- alphabet. For example, let's say we are using the alphabet "ATCG" to encode
-- some data. Here the letter type is `Char` so our separator must also be a
-- character, but it cannot be one of "ATCG" since those characters are used to
-- encode actual data. We could use '_' or '-' for instance.
--
-- >>> encodeSeq '-' (Alphabet "ATCG") [183, 34, 92, 100]
-- "CGTG-CAC-TTGA-TCTA"
--
encodeSeq :: (Eq a, Encode e) => a -> A.Alphabet a -> [e] -> [a]
encodeSeq sep alphabet = intercalate [sep] . map (encode alphabet)

-- | Encodes a pair of values into a list of letters in a specific alphabet 
-- where the encoded values are separated by a special letter.
encode2 :: (Eq a, Encode e, Encode d) => a -> A.Alphabet a -> (e, d) -> [a]
encode2 sep alphabet (l, r) = l' ++ [sep] ++ r'
 where
  l' = encode alphabet l
  r' = encode alphabet r

-- | Encodes a sequence of values into a space-separated string using a `Char`
-- alphabet.
--
-- For this function to work correctly, the alphabet being used should not
-- contain the ' ' character.
--
-- >>> encodeWords (Alphabet "abcd") [183, 34, 92, 100]
-- "cdbd cac bbda bcba"
--
encodeWords :: Encode e => A.Alphabet Char -> [e] -> String
encodeWords = encodeSeq ' '

-- | The inverse of `encodeSeq`; decodes a list of letters into values using
-- a predefined letter as the delimiter between encoded values.
--
-- As with `encodeSeq` the letter used for `sep` must not appear in the 
-- alphabet used to decode values.
--
-- Returns an error if the list being decoded contains letters not present
-- in the alphabet (or the separator).
--
-- >>> runExcept $ decodeSeq '-' (Alphabet "ATCG") "CGTG-CAC-TTGA-TCTA"
-- Right [183,34,92,100]
--
decodeSeq
  :: (Eq a, Show a, Typeable a, Encode e)
  => a
  -> A.Alphabet a
  -> [a]
  -> Except Error [e]
decodeSeq sep alphabet = mapM (decode alphabet) . splitOn [sep]

-- | The inverse of `encode2`; decodes a list of letters into two values using
-- a predefined letter as the delimiter between them.
--
-- Returns an error if the encoded list contains more (or less) than two values
-- or if it contains any letters not contained in the alphabet.
decode2
  :: (Eq a, Show a, Typeable a, Encode e, Encode d)
  => a
  -> A.Alphabet a
  -> [a]
  -> Except Error (e, d)
decode2 sep alphabet src = case splitOn [sep] src of
  [l, r] -> do
    l' <- decode alphabet l
    r' <- decode alphabet r
    return (l', r')
  splits -> throwError $ toError (FailedToSplit sep 2 (length splits))

-- | The inverse of `encodeWords`; decodes a space-separated string into values
-- using a character alphabet.
--
-- As with `encodeWords` the alphabet used to decode values must not contain
-- the ' ' character.
--
-- Returns an error if the string being decoded contains any characters not
-- present in the alphabet (or the separator).
--
-- >>> runExcept $ decodeWords (Alphabet "abcd") "cdbd cac bbda bcba"
-- Right [183,34,92,100]
decodeWords :: Encode e => A.Alphabet Char -> String -> Except Error [e]
decodeWords = decodeSeq ' '
