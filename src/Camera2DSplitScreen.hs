{-# LANGUAGE TemplateHaskell #-}

module Camera2DSplitScreen where

import Control.Lens
import Control.Monad
import Raylib.Core
import Raylib.Core.Shapes
import Raylib.Core.Text
import Raylib.Core.Textures
import Raylib.Types
import Raylib.Util
import Raylib.Util.Colors
import Raylib.Util.Lenses
import Raylib.Util.Math

screenWidth :: Int
screenWidth = 800

screenHeight :: Int
screenHeight = 440

title :: String
title = "raylib [core] example - 2d camera split screen"

fps :: Int
fps = 60

maxBuildings :: Int
maxBuildings = 100

playerSize :: Int
playerSize = 40

initPlayer1, initPlayer2 :: Rectangle
initPlayer1 = Rectangle 200 200 (fromIntegral playerSize) (fromIntegral playerSize)
initPlayer2 = Rectangle 250 200 (fromIntegral playerSize) (fromIntegral playerSize)

initCamera :: Camera2D
initCamera =
  Camera2D
    { camera2D'target = zero
    , camera2D'offset = Vector2 200 200
    , camera2D'rotation = 0
    , camera2D'zoom = 1
    }

rectanglePosition :: Rectangle -> Vector2
rectanglePosition (Rectangle x y _ _) = Vector2 x y

data GameState = GameState
  { _player1 :: Rectangle
  , _player2 :: Rectangle
  , _camera1 :: Camera2D
  , _camera2 :: Camera2D
  }
makeLenses 'GameState

initGameState :: GameState
initGameState =
  GameState
    { _player1 = initPlayer1
    , _player2 = initPlayer2
    , _camera1 = initCamera
    , _camera2 = initCamera
    }

loop :: (RenderTexture, RenderTexture) -> GameState -> IO GameState
loop (sc1, sc2) gameState = do
  p1v <- sequenceA [isKeyDown KeyS, isKeyDown KeyW]
  p1h <- sequenceA [isKeyDown KeyD, isKeyDown KeyA]
  p2v <- sequenceA [isKeyDown KeyDown, isKeyDown KeyUp]
  p2h <- sequenceA [isKeyDown KeyRight, isKeyDown KeyLeft]

  let
    direction [True, False] = 1
    direction [False, True] = -1
    direction _ = 0

    newGameState =
      gameState
        & player1 . _rectangle'y +~ direction p1v * 3
        & player1 . _rectangle'x +~ direction p1h * 3
        & player2 . _rectangle'y +~ direction p2v * 3
        & player2 . _rectangle'x +~ direction p2h * 3
        & camera1 . _camera2D'target .~ rectanglePosition (gameState ^. player1)
        & camera2 . _camera2D'target .~ rectanglePosition (gameState ^. player2)

    drawGridAndPlayer = do
      forM_ [0 .. (div screenWidth playerSize)] $
        \i -> drawLine (playerSize * i) 0 (playerSize * i) screenHeight lightGray
      forM_ [0 .. (div screenHeight playerSize)] $
        \i -> drawLine 0 (playerSize * i) screenWidth (playerSize * i) lightGray
      sequence_ $
        (\i j -> drawText (show [i, j]) (10 + playerSize * i) (15 + playerSize * j) 10 lightGray)
          <$> [0 .. div screenWidth playerSize - 1]
          <*> [0 .. div screenHeight playerSize - 1]
      drawRectangleRec (newGameState ^. player1) red
      drawRectangleRec (newGameState ^. player2) blue

  width <- getScreenWidth
  height <- getScreenWidth

  textureMode sc1 $ do
    clearBackground rayWhite
    mode2D (newGameState ^. camera1) drawGridAndPlayer
    drawRectangle 0 0 (width `div` 2) 30 (fade rayWhite 0.6)
    drawText "Player1: W/S/A/D to move" 10 10 10 maroon

  textureMode sc2 $ do
    clearBackground rayWhite
    mode2D (newGameState ^. camera2) drawGridAndPlayer
    drawRectangle 0 0 (width `div` 2) 30 (fade rayWhite 0.6)
    drawText "Player2: UP/DOWN/LEFT/RIGHT to move" 10 10 10 darkBlue

  drawing $ do
    clearBackground black
    let splitScreenRect = Rectangle 0 0 (fromIntegral $ sc1 ^. _renderTexture'texture . _texture'width) (-(fromIntegral $ sc1 ^. _renderTexture'texture . _texture'height))
    drawTextureRec (sc1 ^. _renderTexture'texture) splitScreenRect zero white
    drawTextureRec (sc2 ^. _renderTexture'texture) splitScreenRect (Vector2 (fromIntegral width / 2) 0) white
    drawRectangle (width `div` 2 - 2) 0 4 height lightGray

  pure newGameState

main :: IO ()
main = do
  withWindow screenWidth screenHeight title fps $
    \window -> do
      let semi = loadRenderTexture (screenWidth `div` 2) screenHeight
      [screenCamera1, screenCamera2] <- managed window $ sequenceA [semi, semi]
      void $ whileWindowOpen (loop (screenCamera1, screenCamera2)) initGameState
