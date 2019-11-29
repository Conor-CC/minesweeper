module UserInterface
    (
      setup
    ) where

{-# LANGUAGE ForeignFunctionInterface #-}


import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as Core
import Minegrid

setup :: [[Cell]] -> Window -> UI ()
setup board window = do
        grid <- (getElementsByClassName window ("board_active"))
        msg <- (getElementsByClassName window ("lost_message"))
        btn <- (getElementsByClassName window ("reset_btn"))
        if (length grid > 0 && length msg > 0 && length btn > 0)
          then do
            deleteGrid (head grid)
            deleteGrid (head msg)
            deleteGrid (head btn)
          else return ()
        return window # set title "Minesweepin Paddys"
        let originalRows = createGrid board window
        getBody window #+ originalRows
        return ()

runGame :: [[Cell]] -> Window -> UI ()
runGame newBoard window = do
          grid <- (getElementsByClassName window ("board_nonactive"))
          if (length grid > 0)
              then do
                  deleteGrid (head grid)
              else return ()
          active <- (getElementsByClassName window ("board_active"))
          if (length active > 0)
              then do
                  deleteGrid (head active)
              else return ()
          return window # set title "Minesweepin Paddys"
          let rows = createGameRunningGrid newBoard window
          getBody window #+ rows
          return ()

deleteGrid grid = do
        delete grid

createGrid b w = do
  rows <- [grid (generateGrid b [[]] b w) # set (attr "class") ("board_nonactive")
                                          # set (attr "value") ("noclick")]
  return rows

createGameRunningGrid b w = do
  rows <- [grid (generateGrid b [[]] b w) # set (attr "class") ("board_active")
                                          # set (attr "value") ("noclick")]
  return rows


-- generateGrid and generateRow create a frontend representation of [[Cell]]
generateGrid [] ys b w = ys
generateGrid (x:xs) ys b w = generateGrid xs ((generateRow x [] b w):ys) b w

-- genrateRow
generateRow [] ys b w = ys
generateRow (x:xs) ys b w = generateRow xs ((makeButton x b w):ys) b w

-- Creates a button to be used in the browser minesweeper game board
makeButton :: Cell -> [[Cell]] -> Window -> UI Element
makeButton cell board w = do
        activeGameList <- getElementsByClassName w "board_active"
        -- Assign variables concerned with relevant cell
        let matchingCell = cell
        let mine = hasMine matchingCell
        let cellId = cId matchingCell
        let clicked = isClicked matchingCell
        let activeGame = do
                    if (length activeGameList >= 0)
                      then True
                      else False
        let prox = proximity matchingCell
        let proxString = do
                    if (clicked == True)
                      then (show prox)
                      else "?"
        let color = do
                    if (proxString /= "?")
                      then "#00FFFF"
                      else "A9A9A9"

        -- Define baseline cell attributes
        button <- UI.button # set text proxString
                            # set value proxString
                            # set (attr "id") (show cellId)
                            # set (attr "oncontextmenu") ("return false;")
                            # set (attr "class") ("cell")
                            # set (attr "style") (("background-color:" ++ color ++ ";"))



        on UI.click button $ \_ -> do
                active <- getElementsByClassName w "board_active"
                if (length active <= 0)
                  then do
                      let newBoard = forceClearById board matchingCell
                      runGame newBoard w
                      runFunction $ generateClick cellId
                      liftIO $ print "New Game Started!!"
                      -- liftIO $ print newBoard
                    -- Get the neighbours, if any of them are marked as mines,
                    -- Override the larry.
                  else do
                      case mine of
                        True -> do
                          element button # set text "M"
                                         # set (attr "value") "M"
                                         # set (attr "style") (("background-color:#DC143C;"))
                          getBody w #+ [UI.h1 #+ [string "YOU LOST"] # set (attr "class") ("lost_message")]
                          reset <- UI.button # set text "Reset?" # set (attr "class") ("reset_btn")
                          getBody w #+ [element reset]
                          on UI.click reset $ \_ -> do
                                     setup board w
                        False -> do
                          element button # set text proxString
                                         # set (attr "style") (("background-color:#00FFFF;"))
                          let new = updateAndCheckWin board matchingCell
                          runGame new w
                          liftIO $ print $  "Board Redraw"


        on UI.contextmenu button $ \_ -> do
                element button # set text "F"
        on UI.hover button $ \_ -> do
                element button # set style [("color", "#DC143C")]
        on UI.leave button $ \_ -> do
                element button # set style [("color", "#000000")]

        return button


generateClick :: Int -> JSFunction ()
generateClick = ffi "var cell = document.getElementById(%1); if (cell.value === '?') {cell.dispatchEvent(new Event('click'));}"
