{-# LANGUAGE TemplateHaskell #-}

module Camera2DPlatformer where

import Control.Lens
import Control.Monad
import Raylib.Core
import Raylib.Core.Shapes
import Raylib.Core.Text
import Raylib.Types
import Raylib.Util
import Raylib.Util.Colors
import Raylib.Util.Lenses
import Raylib.Util.Math
import System.IO.Unsafe

screenWidth :: Int
screenWidth = 800

screenHeight :: Int
screenHeight = 450

title :: String
title = "raylib [core] example - 2d camera platformer"

fps :: Int
fps = 60

data Player = Player
  { _position :: !Vector2
  , _speed :: !Float
  , _canJump :: !Bool
  }
makeLenses 'Player

data EnvItem = EnvItem
  { _rect :: !Rectangle
  , _blocking :: !Bool
  , _color :: !Color
  }
makeLenses 'EnvItem

initPlayer :: Player
initPlayer =
  Player
    { _position = Vector2 400 280
    , _speed = 0
    , _canJump = False
    }

envItems :: [EnvItem]
envItems =
  [ EnvItem {_rect = Rectangle 0 0 1000 400, _blocking = False, _color = lightGray}
  , EnvItem {_rect = Rectangle 0 400 1000 200, _blocking = True, _color = gray}
  , EnvItem {_rect = Rectangle 300 200 400 10, _blocking = True, _color = gray}
  , EnvItem {_rect = Rectangle 250 300 100 10, _blocking = True, _color = gray}
  , EnvItem {_rect = Rectangle 650 300 100 10, _blocking = True, _color = gray}
  ]

initCamera :: Camera2D
initCamera =
  Camera2D
    { camera2D'target = zero
    , camera2D'offset = zero
    , camera2D'rotation = 0
    , camera2D'zoom = 1
    }

type CameraState = Maybe Float
type CameraUpdater = Player -> [EnvItem] -> Float -> Int -> Int -> CameraState -> Camera2D -> (CameraState, Camera2D)
type CameraUpdaterSimple = Player -> [EnvItem] -> Float -> Int -> Int -> Camera2D -> Camera2D

fromSimple :: CameraUpdaterSimple -> CameraUpdater
fromSimple f player es delta width' height' _ camera = (Nothing, f player es delta width' height' camera)

cameraCenter :: CameraUpdaterSimple
cameraCenter player _ _ width' height' camera =
  camera
    & _camera2D'offset . _vector2'x .~ width / 2
    & _camera2D'offset . _vector2'y .~ height / 2
    & _camera2D'target .~ player ^. position
  where
    width = fromIntegral width'
    height = fromIntegral height'

cameraCenterInsideMap :: CameraUpdaterSimple
cameraCenterInsideMap player es delta width' height' camera =
  camera
    & _camera2D'offset . _vector2'x
      .~ ( if minV ^. _vector2'x > 0
            then width / 2 - minV ^. _vector2'x
            else
              if maxV ^. _vector2'x < width
                then width - (maxV ^. _vector2'x - width / 2)
                else width / 2
         )
    & _camera2D'offset . _vector2'y
      .~ ( if minV ^. _vector2'y > 0
            then height / 2 - minV ^. _vector2'y
            else
              if maxV ^. _vector2'y < height
                then height - (maxV ^. _vector2'y - height / 2)
                else height / 2
         )
    & _camera2D'target .~ player ^. position
  where
    width = fromIntegral width'
    height = fromIntegral height'
    minX =
      maybe 1000 (min 1000) $
        minimumOf (traverse . rect . _rectangle'x) es
    maxX =
      maybe (-1000) (max (-1000)) $
        maximumOf (traverse . rect . to (\f -> f ^. _rectangle'x + f ^. _rectangle'width)) es
    minY =
      maybe 1000 (min 1000) $
        minimumOf (traverse . rect . _rectangle'y) es
    maxY =
      maybe (-1000) (max (-1000)) $
        maximumOf (traverse . rect . to (\f -> f ^. _rectangle'y + f ^. _rectangle'height)) es
    maxV = unsafePerformIO $ getWorldToScreen2D (Vector2 maxX maxY) cameraCentered
    minV = unsafePerformIO $ getWorldToScreen2D (Vector2 minX minY) cameraCentered
    cameraCentered = cameraCenter player es delta width' height' camera

cameraCenterSmoothFollow :: CameraUpdaterSimple
cameraCenterSmoothFollow player _ delta width' height' camera =
  camera
    & _camera2D'offset . _vector2'x .~ width / 2
    & _camera2D'offset . _vector2'y .~ height / 2
    & _camera2D'target %~ (|+| (if diffLen > minEffectLength then diffV |* (max minSpeed (fractionSpeed * diffLen) * delta / diffLen) else zero))
  where
    width = fromIntegral width'
    height = fromIntegral height'
    minSpeed = 30
    minEffectLength = 10
    fractionSpeed = 0.8
    diffV = (player ^. position) |-| (camera ^. _camera2D'target)
    diffLen = magnitude diffV

cameraEvenOutOnLanding :: CameraUpdater
cameraEvenOutOnLanding player _ delta width' height' cameraState camera =
  (newCameraState, newCamera)
  where
    width = fromIntegral width'
    height = fromIntegral height'
    cameraCentered =
      camera
        & _camera2D'offset . _vector2'x .~ width / 2
        & _camera2D'offset . _vector2'y .~ height / 2
        & _camera2D'target . _vector2'x .~ player ^. position . _vector2'x
    evenOutSpeed = 700
    evenOutDistance = evenOutSpeed * delta
    cameraTargetY = camera ^. _camera2D'target . _vector2'y
    (newCameraState, newCamera) = case cameraState of
      Nothing ->
        ( if (player ^. canJump) && (player ^. speed == 0) && (player ^. position . _vector2'y /= cameraTargetY)
            then Just (player ^. position . _vector2'y)
            else Nothing
        , cameraCentered
        )
      Just evenOutTarget ->
        if abs (evenOutTarget - cameraTargetY) < evenOutDistance
          then (Nothing, cameraCentered & _camera2D'target . _vector2'y .~ evenOutTarget)
          else
            if evenOutTarget > cameraTargetY
              then (Just evenOutTarget, cameraCentered & _camera2D'target . _vector2'y +~ evenOutDistance)
              else (Just evenOutTarget, cameraCentered & _camera2D'target . _vector2'y -~ evenOutDistance)

cameraPlayerBoundsPush :: CameraUpdaterSimple
cameraPlayerBoundsPush player _ _ width' height' camera =
  camera
    & _camera2D'offset .~ Vector2 ((1 - bboxX) * width / 2) ((1 - bboxY) * height / 2)
    & _camera2D'target . _vector2'x
      .~ ( if player ^. position . _vector2'x > bboxWorldMaxX
            then player ^. position . _vector2'x + bboxWorldMinX - bboxWorldMaxX
            else
              if player ^. position . _vector2'x < bboxWorldMinX
                then player ^. position . _vector2'x
                else camera ^. _camera2D'target . _vector2'x
         )
    & _camera2D'target . _vector2'y
      .~ ( if player ^. position . _vector2'y > bboxWorldMaxY
            then player ^. position . _vector2'y + bboxWorldMinY - bboxWorldMaxY
            else
              if player ^. position . _vector2'y < bboxWorldMinY
                then player ^. position . _vector2'y
                else camera ^. _camera2D'target . _vector2'y
         )
  where
    width = fromIntegral width'
    height = fromIntegral height'
    -- bbbox = Vector2 0.2 0.2
    bboxX = 0.2
    bboxY = 0.2
    (Vector2 bboxWorldMinX bboxWorldMinY) = unsafePerformIO $ getScreenToWorld2D (Vector2 ((1 - bboxX) * width / 2) ((1 - bboxY) * height / 2)) camera
    (Vector2 bboxWorldMaxX bboxWorldMaxY) = unsafePerformIO $ getScreenToWorld2D (Vector2 ((1 + bboxX) * width / 2) ((1 + bboxY) * height / 2)) camera

cameraUpdaters :: [(CameraUpdater, String)]
cameraUpdaters = zip [fromSimple cameraCenter, fromSimple cameraCenterInsideMap, fromSimple cameraCenterSmoothFollow, cameraEvenOutOnLanding, fromSimple cameraPlayerBoundsPush] cameraDescriptions

cameraDescriptions :: [String]
cameraDescriptions =
  [ "Follow player center"
  , "Follow player center, but clamp to map edges"
  , "Follow player center; smoothed"
  , "Follow player center horizontally; update player center vertically after landing"
  , "Player push camera on getting too close to screen edge"
  ]

gravity, playerJumpSpeed, playerHorSpeed :: Float
gravity = 400
playerJumpSpeed = 350
playerHorSpeed = 200

updatePlayerControlHor :: Float -> Player -> IO Player
updatePlayerControlHor delta player = do
  hor <- sequenceA [isKeyDown KeyRight, isKeyDown KeyLeft]
  let horMove = case hor of
        [True, False] -> 1
        [False, True] -> -1
        _ -> 0
  pure $ player & position . _vector2'x +~ horMove * delta * playerHorSpeed

updatePlayerControlJump :: Player -> IO Player
updatePlayerControlJump player = do
  jump <- isKeyPressed KeySpace
  pure $ if player ^. canJump && jump then player & speed .~ (-playerJumpSpeed) & canJump .~ False else player

updatePlayer :: Player -> [EnvItem] -> Float -> IO Player
updatePlayer player' es delta = do
  player <- updatePlayerControlJump =<< updatePlayerControlHor delta player'
  let p = player ^. position
  let hitObstacle =
        filter
          ( \ei ->
              and
                [ ei ^. blocking
                , ei ^. (rect . _rectangle'x) <= (p ^. _vector2'x)
                , ei ^. (rect . _rectangle'x) + (ei ^. (rect . _rectangle'width)) >= p ^. _vector2'x
                , ei ^. (rect . _rectangle'y) >= p ^. _vector2'y
                , ei ^. (rect . _rectangle'y) <= p ^. _vector2'y + (player ^. speed * delta)
                ]
          )

  pure $
    case hitObstacle es of
      [] ->
        player
          & position . _vector2'y +~ (player ^. speed * delta)
          & speed +~ (gravity * delta)
          & canJump .~ False
      (he : _) ->
        player
          & speed .~ 0
          & position . _vector2'y .~ (he ^. (rect . _rectangle'y))
          & canJump .~ True

loop ::
  [EnvItem] ->
  (Player, Camera2D, [(CameraUpdater, String)], CameraState) ->
  IO (Player, Camera2D, [(CameraUpdater, String)], CameraState)
loop es (player, camera, updaters, cameraState) = do
  changeCamera <- isKeyPressed KeyC
  let newUpdaters = if changeCamera then tail updaters <> [head updaters] else updaters

  delta <- getFrameTime
  reset <- isKeyPressed KeyR
  mouseMove <- getMouseWheelMove
  let updateZoom z =
        if reset
          then 1
          else clamp 0.25 3 $ z + mouseMove * 0.05
  width <- fromIntegral <$> getScreenWidth
  height <- fromIntegral <$> getScreenHeight
  let (newCameraState, newCamera) =
        fst (head newUpdaters) player es delta width height cameraState camera
          & _2 . _camera2D'zoom %~ updateZoom

  player' <- updatePlayer player es delta
  let newPlayer = if reset then initPlayer else player'

  drawing $ do
    clearBackground lightGray
    mode2D newCamera $ do
      forM_ es (\e -> drawRectangleRec (e ^. rect) (e ^. color))
      drawRectangleV ((newPlayer ^. position) |-| Vector2 20 40) (Vector2 40 40) red
      drawCircleV (newPlayer ^. position) 5 gold

    drawText "Controls:" 20 20 10 black
    drawText "- Right/Left to move" 40 40 10 darkGray
    drawText "- Space to jump" 40 60 10 darkGray
    drawText "- Mouse Wheel to Zoom in-out, R to reset zoom" 40 80 10 darkGray
    drawText "- C to change camera mode" 40 100 10 darkGray
    drawText "Current camera mode:" 20 120 10 black
    drawText (snd $ head newUpdaters) 40 140 10 darkGray

  return (newPlayer, newCamera, newUpdaters, newCameraState)

main :: IO ()
main = do
  withWindow screenWidth screenHeight title fps $
    const $
      void $
        whileWindowOpen (loop envItems) (initPlayer, initCamera, cameraUpdaters, Nothing)
