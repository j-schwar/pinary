{-# OPTIONS_GHC -fno-cse #-}

module Main where

import           Control.Monad.Except
import           Data.Alphabet
import qualified Data.Radix                    as Radix
import qualified GHC.IO.Handle.FD              as FD
import qualified Input.Args                    as Args
import qualified Input.Translate               as Translate
import           System.Console.CmdArgs
import           System.IO
import           System.Exit

main :: IO ()
main = do
  args <- cmdArgs (modes [Args.translate])
  case args of
    Args.Translate input src dst -> do
      input <- case input of
        Just input -> return input
        Nothing    -> hGetContents FD.stdin
      displayResult $ doTranslate src dst input
    _ -> undefined

-- | Translates `input` from some given source radix to a specified destination
-- radix.
--
-- Returns an error if unable to convert either of the radices to alphabets or
-- if the translation fails.
doTranslate :: Int -> Int -> String -> AlphabetMonad Char String
doTranslate src dst input = do
  srcA <- toDefaultAlphabet src
  dstA <- toDefaultAlphabet dst
  translate srcA dstA input

displayResult :: Show a => AlphabetMonad a String -> IO ()
displayResult m = case runExcept m of
  Left  a -> print a
  Right b -> putStrLn b
