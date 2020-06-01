module Data.Radix
  ( Digits(..)
  , convertRadix
  , fromBase10
  , toBase10
  , fromBinary
  , toBinary
  , fromByteString
  , toByteString
  , digitCount
  , padWithLeadingZeros
  )
where

import qualified Data.Digits                   as D
import           Data.Bit
import qualified Data.ByteString               as Bytes

-- | A sequence of digits in a specific radix.
data Digits = Digits { radix :: Int, digits :: [Integer] }
  deriving (Show, Eq)

instance Semigroup Digits where
  a <> b = Digits r (lhs <> rhs)
    where
      r = radix a
      b' = convertRadix r b
      lhs = digits a
      rhs = digits b'

-- | Performs a radix conversion on some digits.
--
-- Since the current radix is stored along with the digits, only the radix to
-- convert to needs to be supplied to this function.
convertRadix :: Int -> Digits -> Digits
convertRadix to ds@(Digits r d) | to == r   = ds
                                | otherwise = Digits to newDigits
  where newDigits = digitList (toInteger to) . D.unDigits (toInteger r) $ d

-- | Constructs a sequence of base-10 digits from a regular integer.
fromBase10 :: Integer -> Digits
fromBase10 = Digits 10 . digitList 10

-- | Converts a digit sequence into a regular base-10 integer.
toBase10 :: Digits -> Integer
toBase10 = D.unDigits 10 . digits . convertRadix 10

-- | Constructs a sequence of binary digits from a list of bits.
fromBinary :: Bit a => [a] -> Digits
fromBinary = Digits 2 . fmap toIntegerBit

-- | Converts a digit sequence into a binary representation.
toBinary :: Digits -> [Integer]
toBinary = digits . convertRadix 2

-- | Constructs a sequence of base-256 digits (i.e., bytes) from a byte string.
fromByteString :: Bytes.ByteString -> Digits
fromByteString bytes = Digits 256 $ fromIntegral <$> Bytes.unpack bytes

-- | Converts a digit sequence into a string of bytes.
toByteString :: Digits -> Bytes.ByteString
toByteString = Bytes.pack . map fromIntegral . digits . convertRadix 256

-- | The number of digits in a given sequence.
--
-- >>> digitCount $ fromBase10 301
-- 3
--
digitCount :: Digits -> Int
digitCount = length . digits

-- | Pads a digit sequence with leading zeros ensuring that the resultant
-- sequence's length is no less than `len`.
padWithLeadingZeros :: Int -> Digits -> Digits
padWithLeadingZeros len ds@(Digits r d) = Digits r $ replicate needed 0 ++ d
  where needed = len - digitCount ds

digitList :: Integer -> Integer -> [Integer]
digitList r value = case D.digits r value of
  [] -> [0]
  xs -> xs
