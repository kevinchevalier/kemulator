module Main where

import System.Environment
import NES
import Cartridge
import DataTypes
import CPU6502
import Data.Word
import Control.Monad.State
import Util
import Control.Monad.IO.Class
import OpCodes
import Timing

startup :: Operation ()
startup = do
  interrupt Reset



runLoop :: Operation ()
runLoop = do
  -- Get the next command.
  nesTick
  runLoop
  -- Run the command.


push2Test :: Operation ()
push2Test = do
  push2 0xFFCC
  a <- pop2
  liftIO $ printHex16 "Popped" a

               
main :: IO ()
main = do
  args <- getArgs
  c <- loadNESFile (head args)
  case c of 
    Just cart -> do 
           let nes = initNES cart
           putStrLn "Cartridge"
           putStrLn . show . cpu $ nes
           (val', status') <- runStateT startup nes
           putStrLn . show . cpu $ status'
           (val'', status'') <- runStateT runLoop status'
           putStrLn . show . cpu $ status''
                    
    Nothing -> putStrLn "Nothing"
