module Main where

import Lib
import Minegrid
import UserInterface
import System.CPUTime (getCPUTime)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.JQuery
import Data.List (elemIndex)

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
    print $ getCell 2 2 board
    startGUI defaultConfig (setup board)


getCell i j xs = do
        let row = (xs !! i)
        (row !! j)
