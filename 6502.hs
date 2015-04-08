module CPU6502
where

import Data.Word

data CPU6502Status = CPU6502Status{
      status :: Word8
    } deriving Show


startCPU :: CPU6502Status
startCPU = CPU6502Status{ status=0 }
