module Main
where


import System.IO
import Data.Array



main :: IO ()
main = do
  lines <- lift lines $ readFile "trace.trace"
  
