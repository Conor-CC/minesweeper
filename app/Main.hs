module Main where

import Lib
import Minegrid
import System.CPUTime (getCPUTime)

main :: IO ()
main = do
  cpuT <- getCPUTime
  let seed = fromIntegral cpuT
  print "Enter n m difficulty"
  n <- getLine
  m <- getLine
  difficulty <- getLine
  let board = setupBoard (read n :: Int) (read m :: Int) seed (read difficulty :: Float)
  print board
  print $ getElement 0 0 board


getElement i j xs = do
                      let row = (xs !! i)
                      (row !! j)
