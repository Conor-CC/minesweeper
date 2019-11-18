module Main where

import Lib
import Minegrid
import UserInterface
import System.CPUTime (getCPUTime)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.JQuery

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

showMessage :: Window -> UI ()
showMessage window = do
    getBody window #+ [string "Hello, world!"]
    return ()

setup :: [[Cell]] -> Window -> UI ()
setup board window = do
  return window # set title "Minesweepin Paddys"
  let rows = [grid (generateGrid board [[]])]
  getBody window #+ rows
  return ()

refreshGrid = []


generateGrid [] ys = ys
generateGrid (x:xs) ys = generateGrid xs ((generateRow x []):ys)

generateRow [] ys = ys
generateRow (x:xs) ys = generateRow xs ((UI.button # set text "?"):ys)

-- getElement i j xs = do
--                       let row = (xs !! i)
--                       (row !! j)
