module PPU 
where

import DataTypes
import qualified Data.Vector as V
import Data.Word
import Data.Bits
import Control.Monad.State
import Util


startPPU :: PPUStatus
startPPU = PPUStatus{
             tables = V.replicate 0x1000 0,
             pallates = V.replicate 0x20 0,
             objectAttributeMemory = V.replicate 0xFF 0,
             line = 0,
             -- Control Register 1 0x2000
             nmiEnabled = False,
             spriteSize = 8,
             backgroundPatternTableAddress = 0x0000,
             spritePatternTableAddress = 0x0000,
             addressIncrement = 1,
             -- Control Register 2 0x2001
             backgroundColor = Black,
             showSprites = False,
             showBackground = False,
             backgroundClipping = False,
             spriteClipping = False,
             monochrome = False,
             -- Status Register
             vramWriteIgnore = False,
             scanlineSpriteCount = False,
             spriteZeroHit = False,
             vBlank = False,
             -- Addresses
             currentVRAMAddress = 0x0000,
             tempVRAMAddress = 0x0000,
             fineXScroll = 0x00,
             flipFlop = False,
             spriteRamAddress = 0
           }


--  0x2001 - 0779 Stored - fffpcsit
--         fff=Full Background Color 000=Black 001=Red 010=Blue 100=Green
--          p = Sprite Visibility
--          c = Background Visibility
--          s = Sprite Clipping 0=No sprites in left column.
--          i = Background Clipping 0=No background in left column.
--          t = Display type 0=Color 1=B&W
 

boolListToWord8 :: [Bool] -> Word8
boolListToWord8 = foldl shiftAdd 0
    where shiftAdd :: Word8 -> Bool -> Word8
          shiftAdd word val
              | val = flip setBit 0 . flip shiftL 1 $ word
              | otherwise = shiftL word 1


false :: a -> Bool
false _ = False

--  0x2002 - vso-----   - v and the PPU address is reset on read.
--          v = Vertical blank has started
--          s = Sprite 0 Hit
--          0 = Sprite overflow, more than 8 on a line.
getPPURegister3 :: Operation Word8
getPPURegister3 = 
    do
      ppuStatus <- getPPUStatus
      let reg = ppuRegister3 ppuStatus
          ppuStatus' = ppuStatus{vBlank=False, flipFlop=False}
      setPPUStatus ppuStatus'
      return reg

ppuRegister3 :: PPUStatus -> Word8
ppuRegister3 status = 
    let fns :: [PPUStatus -> Bool]
        fns = [vBlank,
               spriteZeroHit,
               scanlineSpriteCount,
               false, false, false,
               false, false]
    in boolListToWord8 . map ($ status) $ fns


getPPUStatus :: Operation PPUStatus
getPPUStatus = do
  status <- get
  return $ ppu status

setPPUStatus :: PPUStatus -> Operation ()
setPPUStatus ppuStatus = do
  status <- get
  put status{ppu=ppuStatus}

setPPURegister :: (PPUStatus -> Word8 -> PPUStatus) -> Word8 -> Operation ()
setPPURegister registerFn reg = do
  ppuStatus <- getPPUStatus
  setPPUStatus . flip registerFn reg $ ppuStatus
  showBackground <- liftM showBackground getPPUStatus
  if showBackground 
      then liftIO $ putStrLn "show background" 
      else return ()

-- Control register write 0x2000
--  0x2000 - 0778 Stored - vMsbpiNN
--          v = Execute NMI on VBlank
--          M = PPU Selection (unused)
--          s = Sprite size 0=8x8 1=8x16
--          b = Background pattern table address 0=0x0000 1=0x1000
--          p = Sprite Pattern Table Address     0=0x0000 1=0x1000
--          i = PPU Address Increment 0=1 1=32
--          NN= Name table address 00=0x2000 01=0x2400 10=0x2800 11=0x2C00
ppuRegister1 :: PPUStatus -> Word8 -> PPUStatus
ppuRegister1 status reg = 
    let v = testBit reg 7
        s = if testBit reg 5 then 16 else 8
        b = if testBit reg 4 then 0x1000 else 0x0000
        p = if testBit reg 3 then 0x1000 else 0x0000
        i = if testBit reg 2 then 32 else 1
        n = flip shiftL 11 . fromIntegral $ reg .&. 3
        tempVRAMAddress' = tempVRAMAddress status .|.  n
        in status { nmiEnabled=v,
                    spriteSize=s,
                    backgroundPatternTableAddress=b,
                    spritePatternTableAddress=p,
                    addressIncrement=i,
                    tempVRAMAddress=tempVRAMAddress'}


-- Control register write 0x2001
--  0x2001 - 0779 Stored - fffpcsit
--         fff=Full Background Color 000=Black 001=Red 010=Blue 100=Green
--          p = Sprite Visibility
--          c = Background Visibility
--          s = Sprite Clipping 0=No sprites in left column.
--          i = Background Clipping 0=No background in left column.
--          t = Display type 0=Color 1=B&W
ppuRegister2 :: PPUStatus -> Word8 -> PPUStatus
ppuRegister2 status reg = 
    let p = testBit reg 4
        c = testBit reg 3
        s = testBit reg 2
        i = testBit reg 1
        t = testBit reg 0
        f = case shiftL reg 5  of
              0 -> Black
              1 -> Red
              2 -> Blue
              4 -> Green
        in status { backgroundColor=f,
                    showSprites=p,
                    showBackground=c,
                    backgroundClipping=i,
                    spriteClipping=s,
                    monochrome=t }
                        



-- Address register 0x2006
setPPUVRAMAddress :: Word8 -> Operation ()
setPPUVRAMAddress addr = do
  ppuStatus <- getPPUStatus
  let currentAddr = tempVRAMAddress ppuStatus
      address' = case flipFlop ppuStatus of 
                   False -> (currentAddr .&. 0xFF) .|. (flip shiftL 8 . fromIntegral $ addr) .&. 0x3FFF
                   True -> (currentAddr .&. 0xFF00) .|. (fromIntegral addr)
  setPPUStatus ppuStatus{ tempVRAMAddress=address', flipFlop=not . flipFlop $ ppuStatus }

writePPUVRAMRegister :: Word8 -> Operation ()
writePPUVRAMRegister val = do
  address <- liftM tempVRAMAddress getPPUStatus
  writePPUVRAM address val
  ppuStatus <- getPPUStatus
  setPPUStatus ppuStatus{ tempVRAMAddress=address + 1 }

writePPUVRAM :: Address -> Word8 -> Operation ()
writePPUVRAM address val 
    | (address >= 0x2000) && (address < 0x3F00) = do
       ppuStatus <- getPPUStatus
       setPPUStatus ppuStatus{ tables=tables ppuStatus V.// [(fromIntegral (mod address 0x1000), val)] }
    | (address >= 0x3F00) && (address < 0x4000) = do
       ppuStatus <- getPPUStatus
       setPPUStatus ppuStatus{ pallates=pallates ppuStatus V.// [(fromIntegral (mod address 0x20), val)] }
    | otherwise = do
          liftIO $ printHex "Undefined VRAM write" val
          liftIO $ printHex16 "to" address
          undefined



-- Scroll register 0x2005
writePPUScroll :: Word8 -> Operation ()
writePPUScroll val = do
  ppuStatus <- getPPUStatus
  let currentAddr = tempVRAMAddress ppuStatus
      ppuStatus' = case flipFlop ppuStatus of 
                   False -> writePPUScroll1 ppuStatus val
                   True -> writePPUScroll2 ppuStatus val
  setPPUStatus ppuStatus'
  
writePPUScroll1 :: PPUStatus -> Word8 -> PPUStatus
writePPUScroll1 status val = 
    let maskedVRAMAddress = tempVRAMAddress status .&. 0xFFE0
        fineX = 0x7 .&. val
        coarseX = fromIntegral . shiftR val $ 3
    in status{ tempVRAMAddress=maskedVRAMAddress .|. coarseX, fineXScroll=fineX, flipFlop=True }

writePPUScroll2 :: PPUStatus -> Word8 -> PPUStatus
writePPUScroll2 status val = 
    let maskedVRAMAddress = tempVRAMAddress status .&. 0x0C1F
        fineY = flip shiftL 12 . fromIntegral $ 0x7 .&. val
        coarseY = flip shiftL 2 . fromIntegral $ 0xF8 .&. val
    in status{ tempVRAMAddress=maskedVRAMAddress .|. coarseY .|. fineY, flipFlop=False }


-- OAM Address 0x2003
writeOAMAddress :: Word8 -> Operation ()
writeOAMAddress val = do
  ppuStatus <- getPPUStatus
  setPPUStatus ppuStatus{ spriteRamAddress=val }


