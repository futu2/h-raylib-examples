module InputMouseWheel where

import Control.Monad
import Raylib.Core
import Raylib.Core.Shapes
import Raylib.Core.Text
import Raylib.Util
import Raylib.Util.Colors

screenWidth :: Int
screenWidth = 800

screenHeight :: Int
screenHeight = 450

title :: String
title =  "raylib [core] example - input mouse wheel"

fps :: Int
fps = 60

initBoxPositionY :: Float
initBoxPositionY = fromIntegral screenHeight / 2 -40

scrollSpeed :: Float
scrollSpeed = 4

loop :: Float -> IO Float
loop boxPositionY = do
  mouseWheelMove <- getMouseWheelMove
  let newBoxPositionY = mouseWheelMove * scrollSpeed + boxPositionY
  drawing $ do
    clearBackground rayWhite
    drawRectangle (round $ fromIntegral screenWidth / 2 - 40) (round newBoxPositionY) 80 80 maroon
    drawText "Use mouse wheel to move the cube up and down!" 10 10 20 darkGray
    drawText ("Box position Y: " <> show (round newBoxPositionY)) 10 40 20 lightGray
  return newBoxPositionY

main :: IO ()
main =
  withWindow screenWidth screenHeight title fps $
    const $
      void $
        whileWindowOpen loop initBoxPositionY