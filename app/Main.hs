module Main where

import           Graphics.UI.Threepenny.Core
import           UserInterface

-- Root process of program. If desired, can be modified to take nxm grid sizes
-- and custom dificulty values between 0 and 1.
main :: IO ()
main = do
    startGUI defaultConfig setup
    return ()
