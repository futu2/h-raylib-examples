module Main where

import BasicWindow
import InputKeys
import InputMouse
import InputMouseWheel
import InputGestures
import Camera2D
import Camera2DMouseZoom
import Camera2DPlatformer
import Camera2DSplitScreen
import Data.Foldable (traverse_)

exampleList :: [(String, IO ())]
exampleList =
  [ ("basic window", BasicWindow.main)
  , ("input keys", InputKeys.main)
  , ("input mouse", InputMouse.main)
  , ("input mouseWheel", InputMouseWheel.main)
  , ("input gesture", InputGestures.main)
  , ("camera 2d", Camera2D.main)
  , ("camera 2d mouse zoom", Camera2DMouseZoom.main)
  , ("camera 2d platformer", Camera2DPlatformer.main)
  , ("camera 2d split screen", Camera2DSplitScreen.main)
  ]

main :: IO ()
main = do
  putStrLn "Examples available:"
  traverse_ (\(k, v) -> putStrLn $ show k <> ". " <> v) $ zip [1 ..] (fst <$> exampleList)
  putStrLn "Input number to select example:"
  l :: Int <- read <$> getLine
  snd $ exampleList !! (l - 1)
