module Controller
where

import Data.Bits
import Data.Word
import DataTypes
import Control.Monad.State

startController :: ControllerStatus
startController = ControllerStatus{
                    shiftRegisterStrobe=False,
                    aButton=False, bButton=False, selectButton=False, startButton=False,
                    upButton=False, downButton=False, leftButton=False, rightButton=False }

setControllerStrobe :: Word8 -> Operation ()
setControllerStrobe val = 
    do 
      controllerStatus <- getControllerStatus
      setControllerStatus controllerStatus{ shiftRegisterStrobe=testBit val 1}
      

getControllerStatus :: Operation ControllerStatus
getControllerStatus = liftM controller get

setControllerStatus :: ControllerStatus -> Operation ()
setControllerStatus controllerStatus = do
  status <- get
  put status{ controller = controllerStatus }


readControllerRegister :: Operation Word8
readControllerRegister = do
  return 0x00

setController2Strobe :: Word8 -> Operation ()
setController2Strobe val = return ()

