module APU
where

import DataTypes
import Data.Word

setAPUVolume :: Word8 -> Operation ()
setAPUVolume v = return ()

setAPUChannels :: Word8 -> Operation ()
setAPUChannels v = return ()

writeAPUPulse1 :: Word8 -> Operation ()
writeAPUPulse1 v = return ()

writeAPUSweep1 :: Word8 -> Operation ()
writeAPUSweep1 v = return ()

writeAPUTimer1 :: Word8 -> Operation ()
writeAPUTimer1 v = return ()

writeAPUCounter1 :: Word8 -> Operation ()
writeAPUCounter1 v = return ()

writeAPUPulse2 :: Word8 -> Operation ()
writeAPUPulse2 v = return ()

writeAPUSweep2 :: Word8 -> Operation ()
writeAPUSweep2 v = return ()

writeAPUTimer2 :: Word8 -> Operation ()
writeAPUTimer2 v = return ()


writeAPUCounter2 :: Word8 -> Operation ()
writeAPUCounter2 v = return ()

writeAPUTriangleCounter :: Word8 -> Operation ()
writeAPUTriangleCounter v = return ()

writeAPUTriangleTimer :: Word8 -> Operation ()
writeAPUTriangleTimer v = return ()

writeAPUTriangleCounterLoad :: Word8 -> Operation ()
writeAPUTriangleCounterLoad v = return ()
