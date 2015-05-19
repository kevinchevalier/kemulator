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
import System.IO
import Disassembler

startup :: Operation ()
startup = do
  interrupt Reset

runLoop :: Handle -> Operation ()
runLoop handle = do
  -- Get the next command.
  outputCommand handle
  nesTick
  runLoop handle
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
  file <- openFile "trace.trace" WriteMode
  case c of 
    Just cart -> do 
           let nes = initNES cart
           putStrLn "Cartridge"
           putStrLn . show . cpu $ nes
           (val', status') <- runStateT startup nes
           (val'', status'') <- runStateT (runLoop file) status'
           return ()
    Nothing -> putStrLn "Nothing"
