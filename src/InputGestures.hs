module InputGestures where

import Control.Monad
import Data.Foldable
import Raylib.Core
import Raylib.Core.Shapes
import Raylib.Core.Text
import Raylib.Core.Textures
import Raylib.Types
import Raylib.Util
import Raylib.Util.Colors
import Raylib.Util.Math

screenWidth :: Int
screenWidth = 800

screenHeight :: Int
screenHeight = 450

touchArea :: Rectangle
touchArea =
  Rectangle
    220
    10
    (fromIntegral screenWidth - 230)
    (fromIntegral screenHeight - 20)

title :: String
title = "raylib [core] example - input gestures"

fps :: Int
fps = 60

initTouchPosition :: Vector2
initTouchPosition = zero

maxGestureStrings :: Int
maxGestureStrings = 20

loop :: [Gesture] -> IO [Gesture]
loop gs = do
  currentGesture <- getGestureDetected
  touchPosition <- getTouchPosition 0
  let
    lastGesture = if null gs then GestureNone else head gs
    newGestures =
      if checkCollisionPointRec touchPosition touchArea
        && (currentGesture /= GestureNone)
        && (currentGesture /= lastGesture)
        then
          if length gs + 1 >= maxGestureStrings
            then []
            else currentGesture : gs
        else gs

  drawing $ do
    clearBackground rayWhite
    drawRectangleRec touchArea gray
    drawRectangle 225 15 (screenWidth - 240) (screenHeight - 30) rayWhite
    drawText
      "GESTURES TEST AREA"
      (screenWidth - 270)
      (screenHeight - 40)
      20
      (fade gray 0.5)
    drawRectangleLines 10 29 200 (screenWidth - 50) gray
    drawText "DETECTED GESTURES" 50 15 10 gray
    when (currentGesture /= GestureNone) $ drawCircleV touchPosition 30 maroon
    for_ (zip newGestures [0 ..]) $ \(g, i) -> do
      drawRectangle
        10
        (30 + 20 * i)
        200
        20
        (fade lightGray $ if odd i then 0.5 else 0.3)
      drawText
        (show g)
        35
        (36 + 20 * i)
        10
        (if i < length newGestures - 1 then darkGray else maroon)
  return newGestures

main :: IO ()
main =
  withWindow screenWidth screenHeight title fps $
    const $
      void $
        whileWindowOpen loop []
