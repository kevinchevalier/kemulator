module CPU6502
where

import Data.Word
import Data.Int
import Data.Bits
import Memory
import DataTypes
import Control.Monad.State
import Text.Printf
import Numeric
import Util
import Control.Monad.IO.Class


-- Status register getters
c :: CPU6502Status -> Bool
c = flip testBit 0  . p
z :: CPU6502Status -> Bool
z = flip testBit 1 . p
i :: CPU6502Status -> Bool
i = flip testBit 2 . p
d :: CPU6502Status -> Bool
d = flip testBit 3 . p
b :: CPU6502Status -> Bool
b = flip testBit 4 . p
v :: CPU6502Status -> Bool
v = flip testBit 6 . p
n :: CPU6502Status -> Bool
n = flip testBit 7 . p


setBitValue :: (Bits a) => a -> Int -> Bool -> a
setBitValue bits index setClear 
    | setClear = setBit bits index
    | otherwise = clearBit bits index

-- Status register setters
setC :: Bool -> Word8 -> Word8
setC val status = setBitValue status 0 val 
setZ :: Bool -> Word8 -> Word8
setZ val status = setBitValue status 1 val 
setI :: Bool -> Word8 -> Word8
setI val status = setBitValue status 2 val 
setD :: Bool -> Word8 -> Word8
setD val status = setBitValue status 3 val 
setV :: Bool -> Word8 -> Word8
setV val status = setBitValue status 6 val 
setN :: Bool -> Word8 -> Word8
setN val status = setBitValue status 7 val 

startCPU :: CPU6502Status
startCPU = CPU6502Status{ pc=0, a=0, x=0, y=0, sp=255, p=0, counter=0 }

tick :: Int -> Operation ()
tick ticks = do
  status <- get
  let cpuStatus = cpu status
      cpuStatus' = cpuStatus{counter=(counter cpuStatus) + ticks }
  put status{cpu=cpuStatus'}
  return ()

spToAddress :: Word8 -> Address
spToAddress = (+) 0x100 . fromIntegral

push :: Word8 -> Operation ()
push val = do
  liftIO $ printHex "Push" val
  status <- get
  let spointer = sp . cpu $ status
  liftIO $ printHex "at" spointer
  writeMemory (spToAddress spointer) val
  status <- get 
  let cpuStatus  = cpu status
      cpuStatus' = cpuStatus{ sp=spointer-1}
  put status{ cpu=cpuStatus' }
  return ()


push2 :: Word16 -> Operation ()
push2 val = do
  let high = fromIntegral . flip shiftR 8 $ val
      low = fromIntegral . (.&.) 0xFF $ val
  push high
  push low

pop :: Operation Word8
pop = do
  status <- get
  let spointer = (+) 1 . sp . cpu $ status
  val <- readMemory . spToAddress  $ spointer
  status <- get
  let cpuStatus  = cpu status 
      cpuStatus' = cpuStatus{ sp=spointer}
  put status{cpu=cpuStatus'}
  liftIO $ printHex "Popped" val
  liftIO $ printHex "from" spointer
  return val

pop2 :: Operation Word16
pop2 = do
  low <- liftM fromIntegral pop
  high <- liftM fromIntegral pop
  return $ shiftL high 8 .|. low
    

pcl :: CPU6502Status -> Word8
pcl = fromIntegral . flip (.&.) 0xFF . pc
pch :: CPU6502Status -> Word8
pch = fromIntegral . flip shiftR 8 . pc

isSoftwareInterrupt :: Interrupt -> Bool
isSoftwareInterrupt IRQ = True
isSoftwareInterrupt _ = False

interruptAddress :: Interrupt -> Address
interruptAddress IRQ   = 0xFFFE
interruptAddress NMI   = 0xFFFA
interruptAddress Reset = 0xFFFC

interrupt :: Interrupt -> Operation ()
interrupt int = do 
  status <- get
  if isSoftwareInterrupt int && (i . cpu $ status) then 
      return ()
  else
      do
        let cpuStatus = cpu status
        push (pcl cpuStatus)
        push (pch cpuStatus)
        push (p cpuStatus)
        newAddress <- readMemory2 . interruptAddress $ int
        let p'  = setI True . p $ cpuStatus 
            cpuStatus' = cpuStatus{ pc=newAddress, p=p' }
        status <- get
        put status{cpu=cpuStatus'}
        return ()

binary :: Int -> Char
binary 0 = '0'
binary 1 = '1'

instance Show CPU6502Status where
    show status = 
        printf "CPUStatus Counter:%d PC:0x%04X A:0x%02X X:0x%02X Y:0x%02X SP:0x%02X P(nv_bdizc):0b%08b" (counter status) (pc status) (a status) (x status) (y status) (sp status) (p status)


setA :: Word8 -> Operation ()
setA val = do
  status <- get
  let cpuStatus = cpu status
      cpuStatus' = cpuStatus{a=val}
  put status{cpu=cpuStatus'}
            

setX :: Word8 -> Operation ()
setX val = do
  status <- get
  let cpuStatus = cpu status
      cpuStatus' = cpuStatus{x=val}
  put status{cpu=cpuStatus'}

setP :: Word8 -> Operation ()
setP val = do
  status <- get
  let cpuStatus = cpu status
      cpuStatus' = cpuStatus{p=val}
  put status{cpu=cpuStatus'}
            

setPC :: Word16 -> Operation ()
setPC val = do
  status <- get
  let cpuStatus = cpu status
      cpuStatus' = cpuStatus{pc=val}
  put status{ cpu=cpuStatus'}


readPC :: Operation Word8
readPC = do
  status <- get
  let cpuStatus = cpu status
      thisPC = pc cpuStatus
      cpuStatus' = cpuStatus{ pc=(thisPC+1) }
  val <- readMemory thisPC
  put $ status{ cpu=cpuStatus' }
  return val

readPC2 :: Operation Word16
readPC2 = do
  first <- readPC
  second <- readPC
  let high = flip shiftL 8 . fromIntegral $ second :: Word16
  return $ high .|. fromIntegral first

setFlags :: Word8 -> Operation ()
setFlags val = do
  status <- get
  let p' = setZ (val == 0) . setN (testBit val 7) . p . cpu $ status
      cpuStatus' = (cpu status){p=p'}
  put status{cpu=cpuStatus'}
