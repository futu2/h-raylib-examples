module Camera2D where

import Control.Monad
import Raylib.Core
import Raylib.Core.Shapes
import Raylib.Core.Text
import Raylib.Core.Textures (fade)
import Raylib.Types
import Raylib.Util
import Raylib.Util.Colors
import Raylib.Util.Math

import System.Random

screenWidth :: Int
screenWidth = 800

screenHeight :: Int
screenHeight = 450

title :: String
title = "raylib [core] example - 2d camera"

fps :: Int
fps = 60

maxBuildings :: Int
maxBuildings = 100

initPlayer :: Vector2
initPlayer = Vector2 400 280

cameraOnPlayer :: Vector2 -> Float -> Float -> Camera2D
cameraOnPlayer player r z =
  Camera2D
    { camera2D'target = player |+| Vector2 20 20
    , camera2D'offset = Vector2 (fromIntegral screenWidth / 2) (fromIntegral screenHeight / 2)
    , camera2D'rotation = r
    , camera2D'zoom = z
    }

buildings :: IO [(Rectangle, Color)]
buildings = fst <$> foldM f ([], 0) [1 .. maxBuildings]
  where
    f :: ([(Rectangle, Color)], Float) -> Int -> IO ([(Rectangle, Color)], Float)
    f (bs, spacing) _ = do
      w <- randomRIO (50, 200)
      h <- randomRIO (100, 800)
      c <-
        Color
          <$> randomRIO (200, 240)
          <*> randomRIO (200, 240)
          <*> randomRIO (200, 250)
          <*> pure 255
      let n =
            Rectangle
              (-6000 + spacing)
              (fromIntegral screenHeight - 130 - h)
              w
              h
      return ((n, c) : bs, spacing + w)

loop :: [(Rectangle, Color)] -> (Vector2, Float, Float) -> IO (Vector2, Float, Float)
loop bs (player, rot, zoom) = do
  d <- sequenceA [isKeyDown KeyRight, isKeyDown KeyLeft]
  let player' =
        player |+| case d of
          [True, False] -> Vector2 2 0
          [False, True] -> Vector2 (-2) 0
          _ -> zero
  reset <- isKeyPressed KeyR
  r <- sequenceA [isKeyDown KeyA, isKeyDown KeyS]
  let rot' =
        if reset
          then 0
          else
            (\x -> clamp x (-40) 40) $
              rot + case r of
                [True, False] -> 1
                [False, True] -> -1
                _ -> 0
  mouseMove <- getMouseWheelMove
  -- !!TODO: more haskellish function def in Raylib.Math
  let zoom' =
        if reset
          then 1
          else (\x -> clamp x 0.1 3) $ zoom + mouseMove * 0.05
  let newCamera = cameraOnPlayer player' rot' zoom'

  drawing $ do
    clearBackground rayWhite
    mode2D newCamera $ do
      drawRectangle (-6000) 320 13000 8000 darkGray
      forM_ bs $ uncurry drawRectangleRec
      drawRectangleV player' (Vector2 40 40) red
      drawLine
        (round $ vector2'x $ camera2D'target newCamera)
        (-screenHeight * 10)
        (round $ vector2'x $ camera2D'target newCamera)
        (screenHeight * 10)
        green
      drawLine
        (-screenWidth * 10)
        (round $ vector2'y $ camera2D'target newCamera)
        (screenWidth * 10)
        (round $ vector2'y $ camera2D'target newCamera)
        green

    drawText "SCREEN AREA" 640 10 20 red
    drawRectangle 0 0 screenWidth 5 red
    drawRectangle 0 5 5 screenHeight red
    drawRectangle (screenWidth - 5) 5 5 (screenHeight - 10) red
    drawRectangle 0 (screenHeight - 5) screenWidth 5 red

    drawRectangle 10 10 250 113 (fade skyBlue 0.5)
    drawRectangleLines 10 10 250 113 blue

    drawText "Free 2d camera controls:" 20 20 10 black
    drawText "- Right/Left to move Offset" 40 40 10 darkGray
    drawText "- Mouse Wheel to Zoom in-out" 40 60 10 darkGray
    drawText "- A / S to Rotate" 40 80 10 darkGray
    drawText "- R to reset Zoom and Rotation" 40 100 10 darkGray

  return (player', rot', zoom')

main :: IO ()
main = do
  b <- buildings
  withWindow screenWidth screenHeight title fps $
    const $
      void $
        whileWindowOpen (loop b) (initPlayer, 0, 1)