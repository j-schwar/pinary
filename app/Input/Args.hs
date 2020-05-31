{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Input.Args
  ( Argv(..)
  , cmdModes
  )
where

import qualified Data.Alphabet                 as Alphabet
import           System.IO
import           System.Console.CmdArgs

data Argv
  = Translate { input :: Maybe String, src :: Int, dst :: Int }
  | Encode { filename :: Maybe FilePath, output :: Maybe FilePath, base :: Int }
  | Decode { filename :: Maybe FilePath, output :: Maybe FilePath, base :: Int }
  | Compress { filename :: Maybe FilePath, output :: Maybe FilePath, base :: Int, mode :: String }
  | Decompress { filename :: Maybe FilePath, output :: Maybe FilePath, base :: Int, mode :: String }
  deriving (Show, Data, Typeable)

cmdModes :: Argv
cmdModes =
  modes [translate, encode, decode, compress, decompress]
    &= program "pinary"
    &= summary "Pinary v0.1.0"
    &= help "Prints binary data and numbers with different bases"

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

encode :: Argv
encode =
  Encode
      { filename = Nothing &= args &= typFile
      , output   = Nothing &= name "o" &= typFile &= help
                     "File to dump output to (default stdout)"
      , base     = 94 &= name "b" &= typ "[2..94]" &= help "Base to encode to"
      }
    &= help "Encodes the byte-representation of a file in a given base"

decode :: Argv
decode =
  Decode
      { filename = Nothing &= args &= typFile
      , output   = Nothing &= name "o" &= typFile &= help
                     "File to dump output to (default stdout)"
      , base     = 94 &= name "b" &= typ "[2..94]" &= help "Base to encode from"
      }
    &= help "Decodes a file encoded in a given base"

compress :: Argv
compress =
  Compress
      { filename = Nothing &= args &= typFile
      , output   = Nothing &= name "o" &= typFile &= help
                     "File to dump output to (default stdout)"
      , base     = 94 &= name "b" &= typ "[2..94]" &= help
                     "Radix to display output as"
      , mode     = "ascii" &= typ "[ascii|json]" &= help "Compression mode"
      }
    &= help
         "Compresses then encodes the byte-representation of a file in a given base"

decompress :: Argv
decompress =
  Decompress
      { filename = Nothing &= args &= typFile
      , output   = Nothing &= name "o" &= typFile &= help
                     "File to dump output to (default stdout)"
      , base     = 94 &= name "b" &= typ "[2..94]" &= help
                     "Radix to display output as"
      , mode     = "ascii" &= typ "[ascii|json]" &= help "Compression mode"
      }
    &= help
         "Decodes and decompresses an encoded + compressed file"
