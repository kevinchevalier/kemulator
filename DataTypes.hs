{-# LANGUAGE TemplateHaskell #-}

module DataTypes where


import Data.Vector
import Data.Word
import qualified Data.Vector as V
import qualified Data.ByteString as B
import Control.Monad.State
import Control.Lens

type Address = Word16

type MapperRead  = Cartridge -> Address -> Word8
type MapperWrite = Cartridge -> Address -> Word8 -> Cartridge

data NESStatus = NESStatus {
      cartridge :: Cartridge,
      cpu :: CPU6502Status,
      memoryManager :: MemoryManager,
      ppu :: PPUStatus,
      controller :: ControllerStatus
   }

data Interrupt = IRQ | NMI | Reset

data BackgroundColor = Black | Red | Blue | Green
                       deriving Show

data PPUStatus = PPUStatus {
      tables :: V.Vector Word8,
      pallates :: V.Vector Word8,
      spriteRam :: V.Vector Word8,
      line :: Int,
      -- Control Register 1 0x2000
      nmiEnabled :: Bool,
      spriteSize :: Int,
      backgroundPatternTableAddress :: Address,
      spritePatternTableAddress :: Address,
      addressIncrement :: Int,
      -- Control Register 2 0x2001
      backgroundColor :: BackgroundColor,
      showSprites :: Bool,
      showBackground :: Bool,
      backgroundClipping :: Bool,
      spriteClipping :: Bool,
      monochrome :: Bool,
      -- Status Register
      vramWriteIgnore :: Bool,
      scanlineSpriteCount :: Bool,
      spriteZeroHit :: Bool,
      vBlank :: Bool,
      -- Addresses
      currentVRAMAddress :: Address,
      tempVRAMAddress :: Address,
      fineXScroll :: Word8,
      flipFlop :: Bool,
      spriteRamAddress :: Word8
    } deriving (Show)

data CPU6502Status = CPU6502Status {
      pc     :: Address,
      a      :: Word8,
      x      :: Word8,
      y      :: Word8,
      sp     :: Word8,
      p      :: Word8,
      counter:: Int
    }



type Operation = StateT NESStatus IO

data MemoryManager = MemoryManager {
      block :: Vector Word8
}



data Mirroring = FourScreen | Vertical | Horizontal

data Cartridge = EmptyCartridge | Cartridge {
      mirroring :: Mirroring,
      prgPageCount :: Int,
      chrPageCount :: Int,
      prgPages :: [V.Vector Word8],
      chrPages :: [V.Vector Word8],
      mapperRead :: MapperRead,
      mapperWrite :: MapperWrite
} 


type OpCode = Word8 -> Operation Word8
data InputAddressing  = NoInputAddressing  | InputAddressing (Operation Word8)
data OutputAddressing = NoOutputAddressing | OutputAddressing (Word8 -> Operation ())

type Op = (OpCode, InputAddressing, OutputAddressing)

data ControllerStatus = ControllerStatus{
      shiftRegisterStrobe :: Bool,
      aButton :: Bool,
      bButton :: Bool, 
      selectButton :: Bool,
      startButton :: Bool,
      upButton :: Bool,
      downButton :: Bool,
      leftButton :: Bool,
      rightButton :: Bool
}

$(makeLenses ''CPU6502Status)
