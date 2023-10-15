module BasicWindow where

import Raylib.Core
import Raylib.Core.Text
import Raylib.Util
import Raylib.Util.Colors

main :: IO ()
main = do
  let screenWidth = 800
      screenHeight = 450
      title = "raylib [core] example - basic window"
      fps = 60
  withWindow screenWidth screenHeight title fps $
    const $
      whileWindowOpen0 $
        drawing $ do
          clearBackground rayWhite
          drawText "Congrats! You created your first window!" 190 200 20 lightGray