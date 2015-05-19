module Memory
where

import Data.Word
import Data.Bits
import qualified Data.Vector as V
import DataTypes
import Control.Monad.State
import PPU
import APU
import Util
import Controller

startMemory :: MemoryManager
startMemory = MemoryManager{ block=V.replicate 0x800 0}

readMemory :: Address  -> Operation Word8
readMemory address  
    | address < 0x2000 = 
        do 
          status <- get
          return $ (block . memoryManager $ status) V.! fromIntegral (mod address 0x800)
    | address == 0x2002 = getPPURegister3
    | address == 0x4016 = readControllerRegister
    | address == 0x4017 = readControllerRegister
    | address >= 0x6000 && address <= 0xFFFF = 
        do
          status <- get
          let cart = cartridge status
          return $ mapperRead cart cart address  
    | otherwise = do
        liftIO $ printHex16 "Undefined read" address
        undefined

      

readMemory2 :: Address -> Operation Word16
readMemory2 address = do
  first <- readMemory address
  second <- readMemory (address + 1) 
  let high = flip shiftL 8 . fromIntegral $ second :: Word16
  return $ high .|. fromIntegral first

writeMemory :: Address -> Word8 -> Operation ()
writeMemory address val
    | address < 0x2000 = 
        do
          status <- get 
          let mm  = memoryManager status
              mm' = mm { block=(block mm) V.// [(fromIntegral (mod address 0x800), val)]}
          put status{ memoryManager=mm' }
    | address == 0x2000 = setPPURegister ppuRegister1 val
    | address == 0x2001 = setPPURegister ppuRegister2 val
    | address == 0x2003 = writeOAMAddress val
    | address == 0x2005 = writePPUScroll val
    | address == 0x2006 = setPPUVRAMAddress val
    | address == 0x2007 = writePPUVRAMRegister val
    | address == 0x4000 = writeAPUPulse1 val
    | address == 0x4001 = writeAPUSweep1 val
    | address == 0x4002 = writeAPUTimer1 val
    | address == 0x4003 = writeAPUCounter1 val
    | address == 0x4004 = writeAPUPulse2 val
    | address == 0x4005 = writeAPUSweep2 val
    | address == 0x4006 = writeAPUTimer2 val
    | address == 0x4007 = writeAPUCounter2 val
    | address == 0x4008 = writeAPUTriangleCounter val
    | address == 0x400A = writeAPUTriangleTimer val
    | address == 0x400B = writeAPUTriangleCounterLoad val
    | address == 0x4011 = setAPUVolume val
    | address == 0x4014 = startPPUDMA val
    | address == 0x4015 = setAPUChannels val
    | address == 0x4016 = setControllerStrobe val
    | address == 0x4017 = setController2Strobe val
    | otherwise = 
        do
          liftIO $ printHex "Undefined write" val
          liftIO $ printHex16 "to" address
          undefined

-- PPU OAM DMA 0x4014
--  Copy from val=$xx cpu address $xx00-xxFF to PPU address starting at spriteRamAddress
startPPUDMA :: Word8 -> Operation ()
startPPUDMA highAddress = do
  ppuStatus <- getPPUStatus
  let cpuStartAddress = flip shiftL 8 . fromIntegral $ highAddress :: Address
      cpuRamRange = [cpuStartAddress .. cpuStartAddress + 0xFF]
      oamRamRange = [spriteRamAddress  ppuStatus .. (+) 0xFF . spriteRamAddress $ ppuStatus]
  mapM_ copyCPUtoPPU (zip cpuRamRange oamRamRange)
--  tick 514


copyCPUtoPPU :: (Address, Word8) -> Operation ()
copyCPUtoPPU (cpuAddress, ppuOAMAddress) = do
  val <- readMemory cpuAddress
  ppuStatus <- getPPUStatus
  setPPUStatus ppuStatus{ objectAttributeMemory =(objectAttributeMemory ppuStatus) V.// [(fromIntegral ppuOAMAddress, val)] }

               

