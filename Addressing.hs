module Addressing
where

import DataTypes
import Control.Monad.State
import CPU6502
import Memory
import Util
import NES
import Data.Word

-- Input Addressing Types
immediateInput :: InputAddressing
immediateInput = InputAddressing readPC
            
registerInput :: (CPU6502Status -> Word8) -> InputAddressing
registerInput acc = InputAddressing $ do
                      cpuStatus <- getCPUStatus
                      return . acc $ cpuStatus

aInput :: InputAddressing
aInput = registerInput a

xInput :: InputAddressing
xInput = registerInput x

yInput :: InputAddressing
yInput = registerInput y

absoluteInput :: InputAddressing
absoluteInput = InputAddressing $
          do
            address <- readPC2
            --liftIO (printHex16 "Read Address" address)
            readMemory address

-- Absolute can be used for input and output.  In this case, step PC back 2 in order to work properly.
absoluteRegressInput :: InputAddressing
absoluteRegressInput = InputAddressing $
          do
            pcVal <- liftM pc getCPUStatus
            address <- readPC2
            val <- readMemory address
            setPC pcVal
            return val



absoluteXInput :: InputAddressing
absoluteXInput = InputAddressing $
          do
            address <- readPC2
            xReg <- liftM (extendWord8 . x) getCPUStatus
            --liftIO (printHex16 "Read Address" address)
            readMemory $ address + xReg

absoluteModInput :: (CPU6502Status -> Word8) -> InputAddressing
absoluteModInput reg = InputAddressing $
          do
            address <- readPC2
            regVal <- liftM (extendWord8 . reg) getCPUStatus
            --liftIO (printHex16 "Read Address" address)
            readMemory $ address + regVal

absoluteModRegressInput :: (CPU6502Status -> Word8) -> InputAddressing
absoluteModRegressInput reg = InputAddressing $
          do
            pcVal <- liftM pc getCPUStatus
            address <- readPC2
            regVal <- liftM (extendWord8 . reg) getCPUStatus
            --liftIO (printHex16 "Read Address" address)
            cpuStatus <- getCPUStatus
            setCPUStatus cpuStatus{ pc=pcVal }
            readMemory $ address + regVal

-- Read the two bytes at PC on the zero page.  Increment this address by Y and store the result there.
indirectYInput :: InputAddressing
indirectYInput = InputAddressing $
          do
            indirect <- readPC
            address <- readMemory2 . fromIntegral $ indirect
            y <- liftM y getCPUStatus
            let address' = address + fromIntegral y
            readMemory address'

zeroPageInput :: InputAddressing 
zeroPageInput = InputAddressing $ do
            address <- readPC
            -- liftIO (printHex16 "Store Address" . fromIntegral $ address)
            readMemory (fromIntegral address)

zeroPageModInput :: (CPU6502Status -> Word8) ->InputAddressing 
zeroPageModInput reg = InputAddressing $ do
            address <- liftM fromIntegral readPC
            regVal <- liftM (extendWord8 . reg) getCPUStatus
            -- liftIO (printHex16 "Store Address" . fromIntegral $ address)
            readMemory $ address + regVal

zeroPageRegressInput :: InputAddressing 
zeroPageRegressInput = InputAddressing $ do
            pcVal <- liftM pc getCPUStatus
            address <- readPC
            -- liftIO (printHex16 "Store Address" . fromIntegral $ address)
            cpuStatus <- getCPUStatus
            setCPUStatus cpuStatus{ pc=pcVal }
            readMemory (fromIntegral address)

zeroPageModRegressInput :: (CPU6502Status -> Word8) -> InputAddressing 
zeroPageModRegressInput reg = InputAddressing $ do
            pcVal <- liftM pc getCPUStatus
            address <- liftM fromIntegral readPC
            regVal <- liftM (extendWord8 . reg) getCPUStatus
            -- liftIO (printHex16 "Store Address" . fromIntegral $ address)
            cpuStatus <- getCPUStatus
            setCPUStatus cpuStatus{ pc=pcVal }
            readMemory $ address + regVal

popInput :: InputAddressing
popInput = InputAddressing pop

            
-- Output Addressing Types
pOutput :: OutputAddressing
pOutput = OutputAddressing (\val -> 
          do
            cpuStatus <- getCPUStatus
            setCPUStatus cpuStatus{p=val})

aOutput :: OutputAddressing
aOutput = OutputAddressing (\val -> 
          do
            cpuStatus <- getCPUStatus
            setCPUStatus cpuStatus{a=val})

xOutput :: OutputAddressing
xOutput = OutputAddressing (\val -> 
          do
            cpuStatus <- getCPUStatus
            setCPUStatus cpuStatus{x=val})

yOutput :: OutputAddressing
yOutput = OutputAddressing (\val -> 
          do
            cpuStatus <- getCPUStatus
            setCPUStatus cpuStatus{y=val})

spOutput :: OutputAddressing
spOutput = OutputAddressing (\val -> 
          do
            liftIO $ printHex "Set SP" val
            cpuStatus <- getCPUStatus
            setCPUStatus cpuStatus{sp=val})

absoluteOutput :: OutputAddressing 
absoluteOutput = OutputAddressing (\val ->
          do
            address <- readPC2
            -- liftIO (printHex16 "Store Address" address)
            writeMemory address val)

absoluteModOutput :: (CPU6502Status -> Word8) -> OutputAddressing 
absoluteModOutput reg = OutputAddressing (\val ->
          do
            address <- readPC2
            regVal <- liftM (extendWord8 . reg) getCPUStatus
            let address' = address + fromIntegral regVal
            -- liftIO (printHex16 "Store Address" address)
            writeMemory address' val)

zeroPageOutput :: OutputAddressing 
zeroPageOutput = OutputAddressing (\val ->
          do
            address <- readPC
            -- liftIO (printHex16 "Store Address" . fromIntegral $ address)
            writeMemory (fromIntegral address) val)

zeroPageModOutput :: (CPU6502Status -> Word8) -> OutputAddressing 
zeroPageModOutput reg = OutputAddressing (\val ->
          do
            address <- liftM fromIntegral readPC
            regVal <- liftM (extendWord8 . reg) getCPUStatus
            let address' = address + fromIntegral regVal
            -- liftIO (printHex16 "Store Address" . fromIntegral $ address)
            writeMemory address' val)

-- Read the two bytes at PC on the zero page.  Increment this address by Y and store the result there.
indirectYOutput :: OutputAddressing
indirectYOutput = OutputAddressing (\val ->
          do
            indirect <- readPC
            address <- readMemory2 . fromIntegral $ indirect
            y <- liftM y getCPUStatus
            let address' = address + fromIntegral y
            writeMemory address' val
                                   )

branchOutput :: OutputAddressing
branchOutput = OutputAddressing $ \val ->
               do
                 cpuStatus <- getCPUStatus
                 let pc' = pc cpuStatus + extendWord8 val
                 setCPUStatus cpuStatus{pc=pc'}


pushOutput :: OutputAddressing
pushOutput = OutputAddressing push
           
             
