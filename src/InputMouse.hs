module InputMouse where

import Control.Monad
import Raylib.Core
import Raylib.Core.Shapes
import Raylib.Core.Text
import Raylib.Types
import Raylib.Util
import Raylib.Util.Colors

screenWidth :: Int
screenWidth = 800

screenHeight :: Int
screenHeight = 450

title :: String
title = "raylib [core] example - mouse input"

fps :: Int
fps = 60

initBallPosition :: Vector2
initBallPosition = Vector2 (-100) (-100)

initBallColor :: Color
initBallColor = darkBlue

colorSheet :: MouseButton -> Color
colorSheet b = case b of
  MouseButtonLeft -> maroon
  MouseButtonMiddle -> lime
  MouseButtonRight -> darkBlue
  MouseButtonSide -> purple
  MouseButtonExtra -> yellow
  MouseButtonForward -> orange
  MouseButtonBack -> beige

loop :: (Vector2, Color) -> IO (Vector2, Color)
loop (_, ballColor) = do
  mousePosition <- getMousePosition
  colors <- fmap colorSheet <$> filterM isMouseButtonPressed [minBound..maxBound]
  let newColor = head $ colors <> [ballColor]
  drawing $ do
    clearBackground rayWhite
    drawText "move ball with mouse and click mouse button to change color" 10 10 20 darkGray
    drawCircleV mousePosition 50 newColor
  return (mousePosition, newColor)

main :: IO ()
main =
  withWindow screenWidth screenHeight title fps $
    const $
      void $
        whileWindowOpen loop (initBallPosition, initBallColor)
