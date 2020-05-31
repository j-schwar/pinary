module Data.Radix where

import qualified Data.Digits                   as D
import           Data.Bit
import qualified Data.ByteString               as Bytes

-- | A sequence of digits in a specific radix.
data Digits = Digits { radix :: Int, digits :: [Integer] }
  deriving (Show, Eq)

-- | Performs a radix conversion on some digits.
--
-- Since the current radix is stored along with the digits, only the radix to
-- convert to needs to be supplied to this function.
convertRadix :: Int -> Digits -> Digits
convertRadix to ds@(Digits r d) | to == r   = ds
                                | otherwise = Digits to newDigits
  where newDigits = D.digits (toInteger to) . D.unDigits (toInteger r) $ d

-- | Constructs a sequence of base-10 digits from a regular integer.
fromBase10 :: Integer -> Digits
fromBase10 = Digits 10 . D.digits 10

-- | Converts a digit sequence into a regular base-10 integer.
toBase10 :: Digits -> Integer
toBase10 = D.unDigits 10 . digits . convertRadix 10

-- | Constructs a sequence of binary digits from a list of bits.
fromBinary :: Bit a => [a] -> Digits
fromBinary = Digits 2 . fmap toIntegerBit

-- | Constructs a sequence of base-256 digits (i.e., bytes) from a byte string.
fromByteString :: Bytes.ByteString -> Digits
fromByteString = Digits 256 . Bytes.foldr (\x xs -> fromIntegral x : xs) []

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
