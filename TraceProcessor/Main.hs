module Main
where


import System.IO
import Data.Array
import Control.Monad
import System.Environment(getArgs)
import Data.Word


main :: IO ()
main = do
  traceFileName <- liftM head getArgs
  lines <- liftM lines $ readFile traceFileName
  let output = parseFile lines
  writeFile "out.txt" $ unlines . filter (/= "") . elems $ output
  return ()

parseFile :: [String] -> Array Word16 String
parseFile lines = accumArray accumulator "" (0x8000, 0xFFFF) $ map addressize lines
    where accumulator _ a = a
          addressize :: String -> (Word16, String)
          addressize line = (read . take 6 $ line, line)
