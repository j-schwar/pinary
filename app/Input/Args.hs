{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Input.Args where

import qualified Data.Alphabet                 as Alphabet
import           System.IO
import           System.Console.CmdArgs

data Argv = Translate { input :: Maybe String, src :: Int, dst :: Int }
          | Compress { filename :: Maybe FilePath, base :: Int, mode :: String }
          | Decompress { filename :: Maybe FilePath, base :: Int, mode :: String }
          | Stats { filename :: Maybe FilePath, bases :: [Int] }
          deriving (Show, Data, Typeable)

translate :: Argv
translate =
  Translate
      { input = Nothing &= args &= typ "NUMBER"
      , src   = 10 &= name "s" &= typ "[2..94]" &= help
                  "Source radix (default 10)"
      , dst   = 94 &= name "d" &= typ "[2..94]" &= help
                  "Destination radix (default 94)"
      }
    &= help "Translates a number from some source radix to a target radix"
