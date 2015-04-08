module Operations where

import Data.Word
import Data.Bits
import DataTypes
import CPU6502
import Control.Monad.State
import Control.Monad.IO.Class
import Memory
import Util 
import NES
import Addressing


setFlag :: (Bool -> Word8 -> Word8) -> Bool -> OpCode
setFlag setter setClear val = do
  cpuStatus <- getCPUStatus
  tick 2
  return . setter setClear . p $ cpuStatus

ld :: OpCode
ld val = do
  setFlags val
  tick 2
  return val

st :: OpCode
st val = do
  tick 4
  return val

tx :: OpCode
tx val = do
  tick 2
  return val 

branch :: (CPU6502Status -> Bool) -> OpCode
branch test rel =
    do
      tick 2
      cpuStatus <- getCPUStatus
      if test cpuStatus then
          do

            tick 1
            return rel
      else
          do
            return 0

-- Branch if positive
bpl :: OpCode
bpl = branch $ not . n

bmi :: OpCode
bmi = branch n

-- Branch if carry set.
bcs :: OpCode
bcs = branch $ c

-- Branch if carry clear set.
bcc :: OpCode
bcc = branch $ not . c

-- Branch if not equal.
bne :: OpCode
bne = branch $ not . z

-- Branch if not equal.
beq :: OpCode
beq = branch z

bvs :: OpCode
bvs = branch v




-- Jump to subroutine
-- Push the address - 1 of the return point onto the stack and the set the PC to the memory address.
jsr :: OpCode
jsr val = do
  addr <- readPC2
  currentPC <- liftM pc getCPUStatus
  let currentPC' = currentPC - 1
  push2 currentPC'
  liftIO $ printHex16 "Push jsr" currentPC'
  cpuStatus <- getCPUStatus
  setCPUStatus cpuStatus{pc=addr}
  return 0

-- Return from Interrupt
rti val = do
  pVal <- pop
  address <- pop2
  setPC (address+1)
  setP pVal
  return 0

-- Return from subroutine.
rts :: OpCode
rts val = do
  address <- pop2
  liftIO $ printHex16 "Pop rts" address
  setPC (address+1)
  return 0

-- z,c,n = reg-val
-- c - set if reg >= val
-- z - set if reg = val
-- n - set if bit 7 of result is set
cmp :: (CPU6502Status -> Word8) -> OpCode
cmp reg val = do
  regVal <- liftM reg getCPUStatus
  let res = regVal - val
      carry = regVal > val
  setFlags res
  cpuStatus <- getCPUStatus
  setCPUStatus cpuStatus{p=setC carry . p $ cpuStatus}
  return res
  
-- dec - Decrement
dec :: OpCode
dec val = do
  let newVal = val - 1
  setFlags newVal
  return newVal

-- inc - Increment
inc :: OpCode
inc val = do
  let newVal = val + 1
  setFlags newVal
  return newVal

bit :: OpCode
bit val = do
  cpuStatus <- getCPUStatus
  let aVal = a cpuStatus
      zVal = aVal .&. val == 0
      vVal = testBit val 6
      nVal = testBit val 7
      p' = setZ zVal . setV vVal . setN nVal . p $ cpuStatus
  setCPUStatus cpuStatus{p=p'}
  return 0

logicalOp :: (Word8 -> Word8 -> Word8) -> OpCode
logicalOp fn val = do
  aVal <- liftM a getCPUStatus
  let out = fn aVal val
  setFlags out
  return out

ora :: OpCode
ora = logicalOp (.|.)

eor :: OpCode
eor = logicalOp xor

anda :: OpCode
anda = logicalOp (.&.)

jmpAb :: OpCode
jmpAb val = do
  address <- readPC2
  setPC address
  tick 3
  return 0

jmpInd :: OpCode
jmpInd val = do
  indirect <- readPC2
  address <- readMemory2 indirect
  setPC address
  tick 3
  return 0

lsr :: OpCode
lsr val = 
    do
      let c = testBit val 0
          val' = shiftR val 1
      setFlags val'
      cpuStatus <- getCPUStatus
      let p' = setC c . p $ cpuStatus
      setCPUStatus cpuStatus{ p=p' }
      return val'

asl :: OpCode
asl val = do
  let c' = testBit val 7
      val' = shiftL val 1
  setFlags val'
  cpuStatus <- getCPUStatus
  let p' = setC c' . p $ cpuStatus
  setCPUStatus cpuStatus{ p=p' }
  return val'


rol :: OpCode
rol val = 
    do 
      cpuStatus <- getCPUStatus
      let c' = testBit val 7
          val' = if c cpuStatus then shiftL val 1 else shiftL val 1 .|. 0x80
      setCPUStatus cpuStatus{p=setC c' . p $ cpuStatus}
      setFlags val'
      return val'

ror :: OpCode
ror val = 
    do 
      cpuStatus <- getCPUStatus
      let c' = testBit val 0
          val' = if c cpuStatus then shiftR val 1 else shiftR val 1 .|. 0x80
      setCPUStatus cpuStatus{p=setC c' . p $ cpuStatus}
      setFlags val'
      return val'
          

sbc :: OpCode
sbc val = do
  aVal <- liftM a getCPUStatus
  cVal <- liftM c getCPUStatus
  let cTerm = if cVal then 0 else 1
      val' = aVal - val - cTerm
      vVal = xor (testBit aVal 7) (testBit val 7) && xor (testBit aVal 7) (testBit val' 7) 
      cVal' = aVal >= val + cTerm
  cpuStatus <- getCPUStatus
  setCPUStatus cpuStatus{ p=setC cVal' . setV vVal . p $ cpuStatus}
  setFlags val'
  return val'


adc :: OpCode
adc val = do
  aVal <- liftM a getCPUStatus
  cVal <- liftM c getCPUStatus
  let cTerm = if cVal then 1 else 0
      val' = aVal + val + cTerm
      vVal = not (xor (testBit aVal 7) (testBit val 7)) && xor (testBit aVal 7) (testBit val' 7)  
      total = (fromIntegral aVal + fromIntegral val + fromIntegral cTerm) :: Word16
      cVal' = total > 0xFF 
  cpuStatus <- getCPUStatus
  setCPUStatus cpuStatus{ p=setC cVal' . setV vVal . p $ cpuStatus}
  setFlags val'
  return val'

noop :: OpCode
noop val = undefined
