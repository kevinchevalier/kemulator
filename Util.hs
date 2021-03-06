module Util where

import Data.Word
import Text.Printf
import Data.Bits

formatHex :: Word8 -> String
formatHex val = printf "0x%02X"  val

formatHex2 :: Word16 -> String
formatHex2 val = printf "0x%02X" val

printHex :: String -> Word8 -> IO ()
printHex label val = printf "%s: 0x%02X\n" label val


printHex16 :: String -> Word16 -> IO ()
printHex16 label val = printf "%s: 0x%02X\n" label val

-- Convert Word8 to Word16 but keep 2s complement.
extendWord8 :: Word8 -> Word16
extendWord8 word 
    | testBit word 7 = fromIntegral word .|. 0xFF00
    | otherwise = fromIntegral word
