module InputKeys where

import Control.Monad
import Data.Foldable
import Raylib.Core
import Raylib.Core.Shapes
import Raylib.Core.Text
import Raylib.Types
import Raylib.Util
import Raylib.Util.Colors
import Raylib.Util.Math

-- !!TODO: add sum to Vector
vectorSum :: (Foldable t, Vector a) => t a -> a
vectorSum = foldl' (|+|) zero

screenWidth :: Int
screenWidth = 800

screenHeight :: Int
screenHeight = 450

title :: String
title = "raylib [core] example - keyboard input"

fps :: Int
fps = 60

initBallPosition :: Vector2
initBallPosition =
  Vector2 (fromIntegral screenWidth / 2) (fromIntegral screenHeight / 2)

loop :: Vector2 -> IO Vector2
loop position = do
  changes <-
    vectorSum . fmap snd
      <$> filterM
        (isKeyDown . fst)
        [ (KeyRight, Vector2 2 0)
        , (KeyLeft, Vector2 (-2) 0)
        , (KeyDown, Vector2 0 2)
        , (KeyUp, Vector2 0 (-2))
        ]
  let newPosition = position |+| changes
  drawing $ do
    clearBackground rayWhite
    drawText "move the ball with arrow keys" 10 10 20 darkGray
    drawCircleV newPosition 50 maroon
  return newPosition

main :: IO ()
main =
  withWindow screenWidth screenHeight title fps $
    const $
      void $
        whileWindowOpen loop initBallPosition