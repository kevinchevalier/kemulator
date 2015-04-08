module Timing 
where

import NES
import DataTypes
import PPU
import CPU6502
import Util
import Control.Monad.IO.Class
import OpCodes
import Control.Monad.State


cpuClockFrequency :: Int
cpuClockFrequency = 1789772 -- Cycles per second
ppuLineFrequency :: Int
ppuLineFrequency = 15780 -- (240 lines, 3 undefined, 20 vblank) * 60 fps
ppuClockInterval :: Int
ppuClockInterval = div cpuClockFrequency ppuLineFrequency

nesTick ::  Operation ()
nesTick = do
  initialCycle <- liftM counter getCPUStatus
  cpuTick
  finalCycle <- liftM counter getCPUStatus
  if not $ (div finalCycle ppuClockInterval) == (div initialCycle ppuClockInterval) then
      ppuTick
  else
      return ()

ppuTick :: Operation ()
ppuTick = do
  ppuStatus <- getPPUStatus
  ppuLine $ line ppuStatus

ppuLine :: Int -> Operation ()
ppuLine lineNumber
    | lineNumber == 262 = do
        ppuStatus <- getPPUStatus
        setPPUStatus ppuStatus{ line=0, vBlank=False }
    | lineNumber == 242 = do
        ppuStatus <- getPPUStatus
        setPPUStatus ppuStatus{ line=lineNumber, vBlank=True }
        if nmiEnabled ppuStatus then
            interrupt NMI
        else
            return ()
    | otherwise = do
        ppuStatus <- getPPUStatus
        setPPUStatus ppuStatus{ line=lineNumber + 1 }
               



cpuTick :: Operation ()
cpuTick = do
  command <- readPC
  liftIO $ printHex "Command" command
  let (op, input, output) = getOperation command
  val <- case input of
    InputAddressing inFn -> inFn
    NoInputAddressing -> return 0 
  out <- op val
  case output of
    OutputAddressing outFn -> outFn out
    NoOutputAddressing -> return ()
  status <- get
  liftIO $ putStrLn . show . cpu $ status
  return ()
