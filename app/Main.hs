module Main where

import Lib
import Minegrid
import UserInterface
import System.CPUTime (getCPUTime)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO ()
main = do
    cpuT <- getCPUTime
    let seed = fromIntegral cpuT
    print "Enter n m difficulty"
    n <- getLine
    m <- getLine
    difficulty <- getLine
    let board = setupBoard (read n :: Int) (read m :: Int) seed (read difficulty :: Float)
    startGUI defaultConfig (setup board)
    return ()
