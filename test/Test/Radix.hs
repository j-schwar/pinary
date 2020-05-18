module Test.Radix
  ( run
  )
where

import           Test.QuickCheck
import           Data.Radix

-- | Arbitrary generator for radix values.
--
-- We put a hard upper bound on the radix value to focus testing in this range.
-- Theoretically the system should be able to handle any radix value > 2.
genRadix :: Gen Int
genRadix = elements [2 .. 256]

newtype Radix = Radix { unwrapRadix :: Int }
  deriving (Show)

instance Arbitrary Radix where
  arbitrary = Radix <$> genRadix

  shrink (Radix 2) = []
  shrink (Radix r) = Radix <$> [2 .. r]

-- | Arbitrary generator for digits in a given radix.
--
-- We ensure that the digit value is never equal to, or exceeds the radix value.
genDigit :: Int -> Gen Integer
genDigit r = elements [0 .. (toInteger r - 1)]

instance Arbitrary Digits where
  arbitrary = do
    r  <- genRadix
    ds <- listOf1 $ genDigit r
    return $ Digits r ds

  shrink (Digits 2 [0]) = []
  shrink (Digits r ds) =
    [ Digits r' ds'
    | r'  <- unwrapRadix <$> shrink (Radix r)
    , ds' <- getNonEmpty <$> shrink (NonEmpty ds)
    ]

-- | Convert a digit sequence into an arbitrary radix then back should be
-- equivalent to the identity operation.
--
-- The conversion will strip any leading zeros from the digit sequence so we
-- need to add those back in before comparing.
prop_convert_to_from_is_identity :: Radix -> Digits -> Bool
prop_convert_to_from_is_identity (Radix r) ds = ds == padded_result
 where
  padded_result = padWithLeadingZeros (digitCount ds) result
  result        = convertRadix original . convertRadix r $ ds
  original      = radix ds

-- | `toBase10` should be the inverse function of `fromBase10`.
prop_convert_to_from_base_10_is_identity :: Integer -> Bool
prop_convert_to_from_base_10_is_identity n = n == (toBase10 . fromBase10) n

run :: IO ()
run = do
  quickCheck prop_convert_to_from_is_identity
  quickCheck prop_convert_to_from_base_10_is_identity
