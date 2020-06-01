{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Bit where

-- | Class for types which can be interpreted as a single bit.
class Eq a => Bit a where
  -- | A zero bit.
  zer :: a
  -- | A one bit.
  one :: a
  -- | Converts a bit into an integer value.
  toIntegerBit :: a -> Integer

instance {-# OVERLAPPING #-} Bit Bool where
  zer = False
  one = True
  toIntegerBit a = if a then 1 else 0

instance (Eq a, Integral a) => Bit a where
  zer          = 0
  one          = 1
  toIntegerBit = toInteger
