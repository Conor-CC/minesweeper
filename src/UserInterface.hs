module UserInterface
    (
      setup
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Minegrid (Cell(CellInstance, hasMine, isClicked, cId))
import Data.List (elemIndex)

setup :: [[Cell]] -> Window -> UI ()
setup board window = do
        return window # set title "Minesweepin Paddys"
        let rows = [grid (generateGrid board [[]] board)]
        getBody window #+ rows
        return ()

generateGrid [] ys b = ys
generateGrid (x:xs) ys b = generateGrid xs ((generateRow x [] b):ys) b

generateRow [] ys b = ys
generateRow (x:xs) ys b = generateRow xs ((makeButton x b):ys) b

makeButton :: Cell -> [[Cell]] -> UI Element
makeButton cell board = do

        let matchingCell = cell
        let mine = hasMine matchingCell
        let cellId = cId matchingCell

        button <- UI.button # set text "?"
                            # set (attr "cId") (show cellId)
                            # set (attr "style") ("color:#000000;")

        case (mine) of
            True -> (on UI.click button $ \_ -> do
                    element button # set text "MINE! YOU TWAT.")
            False -> (on UI.click button $ \_ -> do
                    let proximity = findPos board matchingCell -- Find current position, see if neighbours have mines
                    element button # set text (show (genProximity board matchingCell)))

        on UI.hover button $ \_ -> do
                element button # set (attr "style") ("color:#DC143C;")
        on UI.leave button $ \_ -> do
                element button # set (attr "style") ("color:#000000;")
        return button

genProximity board cell = mineNeighbours (iter board cell ((length board) - 1)) board

mineNeighbours :: (Int, Int) -> [[Cell]] -> Int
mineNeighbours (x, y) board = do
                            let topLft = getCell (x - 1) (y - 1) board
                            let top = getCell (x - 1) y board
                            let topRgt = getCell (x - 1) (y + 1) board
                            let rgt = getCell x (y + 1) board
                            let btmRgt = getCell (x + 1) (y + 1) board
                            let btm = getCell (x + 1) y board
                            let btmLft = getCell (x + 1) (y - 1) board
                            let lft = getCell x (y - 1) board
                            countMines (topLft:(top:(topRgt:(rgt:(btmRgt:(btm:(btmLft:(lft:[])))))))) 0

countMines :: [Cell] -> Int -> Int
countMines [] n = n
countMines (x:xs) n | (hasMine x) == True = countMines xs (n + 1)
                    | (hasMine x) == False = countMines xs n

findPos :: [[Cell]] -> Cell -> (Int, Int)
findPos board cell = iter board cell ((length board) - 1)

iter :: [[Cell]] -> Cell -> Int -> (Int, Int)
iter [] _ n = (n, -1)
iter (x:xs) mup n = case (elemIndex mup x) of
                  Just a -> (n, (((length x) - a) - 1))
                  Nothing -> iter xs mup (n - 1)

getCell i j xs = do
                let row = (xs !! i)
                (row !! j)
