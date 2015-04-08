module NES
where

import DataTypes
import Memory
import Control.Monad.State
import PPU
import CPU6502
import Controller


initNES :: Cartridge ->  NESStatus
initNES cart = NESStatus { cartridge = cart, cpu=startCPU, memoryManager=startMemory, ppu=startPPU, controller=startController }

startNES :: Operation ()
startNES = undefined

getCPUStatus :: Operation CPU6502Status
getCPUStatus = do
  status <- get
  return . cpu $ status

setCPUStatus :: CPU6502Status -> Operation ()
setCPUStatus cpuStatus = do
  status <- get
  put $ status{cpu=cpuStatus}



