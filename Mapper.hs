module Mapper where
import DataTypes
import Data.Vector
import Data.Word


-- The null mapper just reads from the PRG-ROM pages. 
-- If there are two pages, then 
defaultRead :: MapperRead
defaultRead cart address = lookup (address-0x8000) (prgPages cart)
    where 
      lookup :: Address -> [Data.Vector.Vector Word8] -> Word8
      lookup a (page:[]) = page ! fromIntegral (mod a 0x4000)
      lookup a (page:rest) 
              | a < 0x4000 = page ! fromIntegral a
              | otherwise = lookup a rest

defaultWrite :: MapperWrite
defaultWrite = undefined
