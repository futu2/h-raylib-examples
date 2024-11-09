module ModelsLoadingGltf where

import Control.Arrow
import Raylib.Core
import Raylib.Util.Lenses

-- import Raylib.Core.Camera
import Raylib.Core.Models
import Raylib.Core.Text
import Raylib.Util
import Raylib.Util.Colors

import Raylib.Types

import Control.Lens

import Linear

main :: IO ()
main = do
  let screenWidth = 800
      screenHeight = 450
      title = "raylib [models] example - loading gltf animations"
      fps = 60
  let camera =
        Camera3D
          { camera3D'position = V3 6 6 6
          , camera3D'target = V3 0 2 0
          , camera3D'up = V3 0 1 0
          , camera3D'fovy = 45
          , camera3D'projection = CameraPerspective
          }
  withWindow screenWidth screenHeight title fps $
    const $ do
      robotModel <- loadModel "resources/gltf/robot.glb"
      robotAnimations <- loadModelAnimations "res/gltf/robot.glb"
      print ((modelAnimation'name &&& modelAnimation'frameCount) <$> robotAnimations)
      let ani1 = robotAnimations !! 0
      let frameCount = ani1 ^. _modelAnimation'frameCount
      whileWindowOpen0 $ do
        updateModelAnimation robotModel ani1 1
        drawing $ do
          clearBackground rayWhite
          mode3D camera $ do
            drawModel robotModel 0 1 white
            drawGrid 10 1
          -- h <- getScreenHeight
          drawText "Use the LEFT/RIGHT mouse buttons to switch animation" 10 10 20 gray
