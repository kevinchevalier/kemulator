module Cartridge
where

import Data.Word
import Data.Maybe
import Data.Char
import qualified Data.Vector as V
import qualified Data.ByteString as B
import DataTypes
import Mapper

toWord8 :: Char -> Word8
toWord8 = fromIntegral . ord

defaultCartridge :: Cartridge
defaultCartridge = Cartridge{mirroring=Vertical,
                            prgPageCount=0,
                            chrPageCount=0,
                            prgPages=[],
                            chrPages=[],
                            mapperRead=defaultRead,
                            mapperWrite=defaultWrite}

hasCorrectIntro :: B.ByteString -> Bool
hasCorrectIntro = (==) (map toWord8 "NES\^Z") . B.unpack . B.take 4 

checkCorrectIntro :: B.ByteString -> Maybe Cartridge
checkCorrectIntro bs =
    if hasCorrectIntro bs then (Just defaultCartridge) else Nothing

byteStringToVec :: B.ByteString -> V.Vector Word8
byteStringToVec = V.fromList . B.unpack

readPages :: Int -> Int -> B.ByteString -> ([V.Vector Word8], B.ByteString)
readPages pageCount size bs = 
    readPage pageCount [] bs
    where 
      readPage 0 pages string = (pages, string)
      readPage left pages string = 
          let (p, rest) = B.splitAt size string in 
          readPage (left - 1) (pages ++ [byteStringToVec p]) rest 


buildCartridge :: B.ByteString -> Maybe Cartridge
buildCartridge bs = do
  cartridge <- checkCorrectIntro bs
  let header = B.unpack . B.take 16 $ bs  
      rest = B.drop 16 bs
      prgPageCount = fromIntegral $ header !! 4
      chrPageCount = fromIntegral $ header !! 5
      (prgPages, rest') = readPages prgPageCount 0x4000 rest
      (chrPages, _) = readPages chrPageCount 8192 rest'
  return cartridge{ prgPages=prgPages, prgPageCount=prgPageCount,
                    chrPages=chrPages, chrPageCount=chrPageCount}
        
  -- Todo                       
  -- Check mirroring.
  -- Check battery backup.d
  -- Check trainer.
  -- Read the mapper number.
  -- Read trainer if present.


-- Take an NES file name, and output a Cartridge.
loadNESFile :: FilePath -> IO (Maybe Cartridge)
loadNESFile filePath = do
  -- Read the file.
  bs <- B.readFile filePath
  -- Check the magic header.
  return $ buildCartridge bs
