{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import qualified Compression                   as Comp
import           Compression.Analysis
import           Compression.Huffman
import qualified Data.ByteString               as Bin
import           Data.Alphabet
import           Data.Radix
import qualified Input.Translate               as Translate
import           System.Console.CmdArgs
import           System.IO
import           System.Exit

data Pinary = Pinary
  { filename :: FilePath
  , alphabet :: String
  , compress :: Bool
  , mode :: String
  }
  deriving (Show, Data, Typeable)

pinary =
  Pinary
      { filename = def &= args &= typFile
      , alphabet =
        "decimal" &= name "a" &= help "The alphabet to encode to" &= typ "NAME"
      , compress = def &= name "c" &= help
                     "Compress the contents of the file before encoding"
      , mode     = "dynamic" &= help "The compression mode to use" &= typ
                     "[dynamic|ascii|json]"
      }
    &= help "Print binary data as text"
    &= summary "Pinary v0.1.0, (c) Jeremy Schwartz 2020"

-- | Transforms a user-supplied alphabet name into an `Alphabet` data object.
--
-- Exits the program with a failure code if unable to lookup the requested
-- alphabet.
getAlphabet :: Pinary -> IO (Alphabet Char)
getAlphabet args = unwrap . Translate.alphabetName . alphabet $ args
 where
  unwrap (Just a) = return a
  unwrap Nothing  = do
    putStrLn $ "Invalid alphabet: " ++ alphabet args
    exitFailure

getCompressionMode :: Pinary -> String -> IO [(Char, Int)]
getCompressionMode args src = unwrap $ Translate.compressionType m src
 where
  m = mode args
  unwrap (Just a) = return a
  unwrap Nothing  = do
    putStrLn $ "Invalid compression mode: " ++ m
    exitFailure

main :: IO ()
main = do
  args  <- cmdArgs pinary
  alpha <- getAlphabet args

  -- Get the digits to encode. If we are compressing the contents of the input
  -- file, then open it as a regular file and compress. If not, open it as a
  -- binary file and convert the ByteString into some digits.
  ds    <- if compress args
    then do
      hdl      <- openFile (filename args) ReadMode
      contents <- hGetContents hdl
      freqList <- getCompressionMode args contents
      let compressor = compressorFromList freqList :: Huffman Integer
      let binary     = Comp.compress compressor contents
      return $ fromBinary binary
    else do
      bytes <- Bin.readFile $ filename args
      return $ fromByteString bytes

  -- Encode and print the result to stdout.
  putStrLn $ encode alpha ds
