module Main where

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
import           Graphics.UI.Threepenny.JQuery
import           Lib
import           Minegrid
import           System.CPUTime                (getCPUTime)
import           UserInterface

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
    startGUI defaultConfig (setup board)
    return ()
