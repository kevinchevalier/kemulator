module Disassembler
where

import DataTypes
import System.IO
import Data.Word
import Util
import Data.List
import Control.Monad.State
import Data.Bits

outputCommand :: Handle -> Operation ()
outputCommand file = do 
  status <- get
  let opcode = pcOffset status 0
  liftIO $ printHex "Opcode" opcode
  liftIO $ hPutStrLn file (opDescription status) >> hFlush file
  liftIO $ putStrLn $ opDescription status
  
type Formatter = NESStatus -> String


pcOffset :: NESStatus -> Word16 -> Word8
pcOffset status offset = mapper cart address 
    where cart = cartridge status 
          mapper = mapperRead cart
          address = (pc . cpu $ status) + offset

pcOffset2 :: NESStatus -> Word16 -> Word16
pcOffset2 status offset = high .|. fromIntegral first
    where cart = cartridge status 
          mapper = mapperRead cart
          address = (pc . cpu $ status) + offset
          first = mapper cart address
          second = mapper cart (address + 1)
          high = flip shiftL 8 . fromIntegral $ second :: Word16

opDescription :: NESStatus -> String
opDescription status = formatHex2 currentPC ++ ": " ++ name ++ " " ++ formatter status 
    where opcode = pcOffset status 0
          (name, formatter) = opcodeLookup opcode
          currentPC = pc . cpu $ status
  
opcodeLookup :: Word8 -> (String, Formatter)
opcodeLookup opcode = case (lookup opcode disopcodes) of 
                        Just o -> o
                        Nothing -> ("ERROR", undefined)


implied :: Formatter
implied _ = ""

absolute :: Formatter
absolute status = formatHex2 $ pcOffset2 status 1 

absoluteMod :: String -> Formatter
absoluteMod mod status = (formatHex2 $ pcOffset2 status 1) ++ "," ++ mod

immediate :: Formatter
immediate status = '#' : (formatHex $ pcOffset status 1)

zeroPage :: Formatter
zeroPage status = formatHex $ pcOffset status 1 

zeroPageMod :: String -> Formatter
zeroPageMod mod status = (formatHex $ pcOffset status 1) ++ "," ++ mod

indirectY :: Formatter
indirectY status = "(" ++ (formatHex $ pcOffset status 1) ++ "),Y"

relative :: Formatter
relative status = formatHex2 $ currentPC + offset + 2
    where currentPC = pc . cpu $ status
          offset = extendWord8 $ pcOffset status 1

accumulator :: Formatter
accumulator _ = "A"

disopcodes :: [(Word8, (String, Formatter))]
disopcodes = [
           (0x05, ("ora", zeroPage)),
           (0x06, ("asl", zeroPage)),
           (0x09, ("ora", immediate)),
           (0x0A, ("asl", accumulator)),
           (0x10, ("bpl", relative)),
           (0x18, ("clc", implied)),
           (0x20, ("jsr", absolute)),
           (0x25, ("and", zeroPage)),
           (0x29, ("and", immediate)),
           (0x2A, ("rol", accumulator)),
           (0x2C, ("bit", absolute)),
           (0x30, ("bmi", relative)),
           (0x35, ("and", zeroPageMod "X")),
           -- (0x3D, ("and", absoluteModInput x, aOutput)),
           (0x38, ("sec", implied)),
           (0x40, ("rti", implied)),
           (0x45, ("eor", zeroPage)),
           (0x46, ("lsr", zeroPage)),
           (0x48, ("pha", implied)),
           (0x49, ("eor", immediate)),
           (0x4A, ("lsr", accumulator)),
           (0x4C, ("jmp", absolute)),
           (0x60, ("rts", implied)),
           (0x65, ("adc", zeroPage)),
           (0x66, ("ror", zeroPage)),
           (0x68, ("pla", implied)),
           (0x69, ("adc", immediate)),
           (0x6A, ("ror", accumulator)),
           -- (0x6C, ("jmp", NoInputAddressing, NoOutputAddressing)),
           (0x6D, ("adc", absolute)),
           (0x70, ("bvs", relative)),
           (0x78, ("sei", implied)),
           -- (0x7E, ("ror", absoluteModRegressInput x, absoluteModOutput x)),
           (0x84, ("sty", zeroPage)),
           (0x85, ("sta", zeroPage)),
           (0x86, ("stx", zeroPage)),
           (0x88, ("dey", implied)),
           (0x8A, ("txa", implied)),
           (0x8C, ("sty", absolute)),
           (0x8D, ("sta", absolute)),
           (0x8E, ("stx", absolute)),
           (0x90, ("bcc", relative)),
           (0x91, ("sta", indirectY)),
           (0x95, ("sta", zeroPageMod "X")),
           (0x98, ("tya", implied)),
           (0x99, ("sta", absoluteMod "Y")),
           (0x9A, ("txs", implied)),
           (0x9D, ("sta", absoluteMod "X")),
           (0xA0, ("ldy", immediate)),
           (0xA2, ("ldx", immediate)),
           (0xA4, ("ldy", zeroPage)),
           (0xA5, ("lda", zeroPage)),
           (0xA6, ("ldx", zeroPage)),
           (0xA8, ("tay", implied)),
           (0xA9, ("lda", immediate)),
           (0xAA, ("tax", implied)),
           (0xAC, ("ldy", absolute)),
           (0xAD, ("lda", absolute)),
           (0xAE, ("ldx", absolute)),
           (0xB0, ("bcs", relative)),
           (0xB1, ("lda", indirectY)),
           (0xB4, ("ldy", zeroPage)),
           (0xB5, ("lda", zeroPageMod "X")),
           (0xB9, ("lda", absoluteMod "Y")),
           (0xBD, ("lda", absoluteMod "X")),
           -- (0xBE, ("ldx", absoluteModInput y, xOutput)),
           (0xC0, ("cpy", immediate)),
           (0xC6, ("dec", zeroPage)),
           (0xC8, ("iny", implied)),
           (0xC9, ("cmp", immediate)),
           (0xCA, ("dex", implied)),
           (0xCE, ("dec", absolute)),
           (0xD0, ("bne", relative)),
           (0xD6, ("dec", zeroPageMod "X")),
           (0xD8, ("cld", implied)),
           -- (0xE0, ("cpx", immediateInput, NoOutputAddressing)),
           (0xE5, ("sbc", zeroPage)),
           (0xE6, ("inc", zeroPage)),
           (0xE8, ("inx", implied)),
           (0xE9, ("sbc", immediate)), 
           (0xEE, ("inc", absolute)),
           (0xF0, ("beq", relative)),
           (0xF9, ("sbc", absoluteMod "Y"))
    ]

