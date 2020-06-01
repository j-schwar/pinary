{-# Language ExistentialQuantification #-}
{-# Language FlexibleInstances #-}
  
module Data.Error where

import Control.Monad.Except
import Data.Typeable

-- | Wrapper for instances of `Exception`.
data Error = forall e. Exception e => Error e

instance Show Error where
  show (Error err) = show err

-- | Class for exception types.
--
-- Allows a type to be wrapped in the `Error` wrapper using the `toError` 
-- function.
class (Typeable e, Show e) => Exception e where
  toError :: e -> Error
  toError = Error

  fromError :: Error -> Maybe e
  fromError (Error e) = cast e

instance Exception [Char]

-- | Generalizes an `ExceptT` monad transformer by up-casting it's error type
-- from some instance of `Exception` to `Error`.
generalizeExceptT :: (Exception e, Functor m) => ExceptT e m a -> ExceptT Error m a
generalizeExceptT = withExceptT toError
