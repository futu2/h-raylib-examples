module Camera2DMouseZoom where

import Control.Arrow
import Control.Lens
import Control.Monad
import Raylib.Core
import Raylib.Core.Models
import Raylib.Core.Shapes
import Raylib.Core.Text
import Raylib.Types
import Raylib.Util
import Raylib.Util.Colors
import Raylib.Util.Lenses
import Raylib.Util.Math
import Raylib.Util.RLGL

screenWidth :: Int
screenWidth = 800

screenHeight :: Int
screenHeight = 450

title :: String
title = "raylib [core] example - 2d camera mouse zoom"

fps :: Int
fps = 60

initCamera :: Camera2D
initCamera =
  Camera2D
    { camera2D'offset = zero
    , camera2D'rotation = 0
    , camera2D'target = zero
    , camera2D'zoom = 1
    }

gameLoop :: Camera2D -> IO Camera2D
gameLoop oldCamera = do
  rightClick <- isMouseButtonDown MouseButtonRight
  f1 <-
    if rightClick
      then do
        delta <- (|* (-1 / oldCamera ^. _camera2D'zoom)) <$> getMouseDelta
        return $ _camera2D'target %~ (|+| delta)
      else return id

  mouseWheel <- getMouseWheelMove
  f2 <-
    if mouseWheel /= 0
      then do
        mousePos <- getMousePosition
        mouseWorldPos <- getScreenToWorld2D mousePos (f1 oldCamera)
        let f =
              _camera2D'offset .~ mousePos
                >>> _camera2D'target .~ mouseWorldPos
                >>> _camera2D'zoom %~ (\z -> max 0.125 (z + mouseWheel * 0.125))
        return f
      else return id

  let newCamera = f2 $ f1 oldCamera

  drawing $ do
    clearBackground black
    mode2D newCamera $ do
      rlPushMatrix
      rlTranslatef 0 (25 * 50) 0
      rlRotatef 90 1 0 0
      drawGrid 100 50
      rlPopMatrix

      drawCircle 100 100 50 yellow

    drawText "Mouse right button drag to move, mouse wheel to zoom" 10 10 20 white

  return newCamera

main :: IO ()
main = do
  withWindow screenWidth screenHeight title fps $
    const $
      void $
        whileWindowOpen gameLoop initCamera