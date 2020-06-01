{-# OPTIONS_GHC -fno-cse #-}

module Main where

import qualified Compression                   as Comp
import           Compression.Huffman            ( compressorFromList )
import           Compression.Analysis
import           Control.Monad.Except
import           Data.Alphabet
import qualified Data.ByteString               as Bytes
import qualified Data.ByteString.UTF8          as UTF8
import           Data.Error
import qualified Data.Radix                    as Radix
import qualified Encode                        as E
import qualified GHC.IO.Handle.FD              as FD
import qualified Args
import           System.Console.CmdArgs
import           System.IO
import           System.Exit

main :: IO ()
main = do
  args <- cmdArgs Args.cmdModes
  case args of
    Args.Translate input src dst -> do
      input <- case input of
        Just input -> return input
        Nothing    -> hGetContents FD.stdin
      displayResult $ doTranslate src dst input

    Args.Encode filename output base -> do
      input <- case filename of
        Just filename -> Bytes.readFile filename
        Nothing       -> Bytes.hGetContents FD.stdin
      writeResult output $ doEncode base input

    Args.Decode filename output base -> do
      input <- getInput filename
      writeByteResult output $ doDecode base input

    Args.Compress filename output base mode -> do
      input <- getInput filename
      ct    <- getCompressionType mode input
      let compressor = compressorFromList ct
      let compressed = Comp.compress compressor input :: [Bool]
      let bytes      = Radix.toByteString $ Radix.fromBinary compressed
      writeResult output $ doEncode base bytes

    Args.Decompress filename output base mode -> do
      input <- getInput filename
      ct    <- getCompressionType mode input
      let compressor = compressorFromList ct
      let binary =
            Radix.toBinary . Radix.fromByteString <$> doDecode base input
      let string = Comp.decompress compressor <$> binary
      case runExcept string of
        Right string -> putStr string
        Left  err    -> print err

-- | Translates `input` from some given source radix to a specified destination
-- radix.
--
-- Returns an error if unable to convert either of the radices to alphabets or
-- if the translation fails.
doTranslate :: Int -> Int -> String -> Except Error String
doTranslate src dst input = generalizeExceptT $ do
  srcA <- toDefaultAlphabet src
  dstA <- toDefaultAlphabet dst
  translate srcA dstA input

-- | Encodes a byte string using the default alphabet for a given radix.
--
-- Returns an error if unable to convert the radix to an alphabet.
doEncode :: Int -> Bytes.ByteString -> Except Error String
doEncode base bytes = do
  alphabet <- generalizeExceptT $ toDefaultAlphabet base
  return $ E.encode alphabet bytes

doDecode :: Int -> String -> Except Error Bytes.ByteString
doDecode base input = do
  alphabet <- generalizeExceptT $ toDefaultAlphabet base
  E.decode alphabet input

getInput :: Maybe FilePath -> IO String
getInput (Just filename) = readFile filename
getInput Nothing         = hGetContents FD.stdin

-- | Displays a result to stdout.
displayResult :: Except Error String -> IO ()
displayResult m = case runExcept m of
  Left  a -> print a
  Right b -> putStrLn b

-- | Writes a result to a specific file or stdout if no file is provided.
writeResult :: Maybe FilePath -> Except Error String -> IO ()
writeResult output m = case runExcept m of
  Right text -> case output of
    Just filename -> writeFile filename text
    Nothing       -> putStrLn text
  Left err -> print err

-- | Writes a byte result to a specific file or stdout if no file is provided.
writeByteResult :: Maybe FilePath -> Except Error Bytes.ByteString -> IO ()
writeByteResult output m = case runExcept m of
  Right bytes -> case output of
    Just filename -> Bytes.writeFile filename bytes
    Nothing       -> putStr $ UTF8.toString bytes
  Left err -> print err

getCompressionType :: String -> String -> IO [(Char, Int)]
getCompressionType mode input = case compressionType mode input of
  Just t  -> return t
  Nothing -> putStrLn ("Invalid mode: " ++ mode) >> exitFailure

-- | Returns the frequency table to use for compression.
compressionType :: String -> String -> Maybe [(Char, Int)]
compressionType "json"    _   = Just jsonFrequency
compressionType "ascii"   _   = Just asciiFrequency
compressionType "dynamic" src = Just $ analyseAsciiFrequency src
compressionType _         _   = Nothing
