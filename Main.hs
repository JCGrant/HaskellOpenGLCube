{-# LANGUAGE RecordWildCards #-}

import           Control.Monad
import           Data.IORef
import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLUT

coloredFace :: (GLfloat, GLfloat, GLfloat) -> IO ()
coloredFace (r, g, b) = do
  materialAmbient Front $= Color4 r g b 1.0
  materialDiffuse Front $= Color4 r g b 1.0
  materialSpecular Front $= Color4 0.5 0.5 0.5 1.0
  materialShininess Front $= 2.0

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

square :: GLfloat -> IO ()
square width =
  renderPrimitive Quads $
  mapM_ vertex3f [(w, w, 0), (w, -w, 0), (-w, -w, 0), (-w, w, 0)]
  where
    w = width / 2

colorCube :: GLfloat -> IO ()
colorCube n = do
  preservingMatrix $ do
    coloredFace (1.0, 0.0, 0.0)
    translate $ Vector3 0 0 (-n / 2)
    square n
  preservingMatrix $ do
    coloredFace (0.0, 1.0, 0.0)
    translate $ Vector3 0 0 (n / 2)
    square n
  preservingMatrix $ do
    coloredFace (0.0, 0.0, 1.0)
    translate $ Vector3 (n / 2) 0 0
    rotate 90 $ Vector3 (0 :: GLfloat) 1 0
    square n
  preservingMatrix $ do
    coloredFace (1.0, 1.0, 0.0)
    translate $ Vector3 (-n / 2) 0 0
    rotate 90 $ Vector3 (0 :: GLfloat) 1 0
    square n
  preservingMatrix $ do
    coloredFace (0.0, 1.0, 1.0)
    translate $ Vector3 0 (-n / 2) 0
    rotate 90 $ Vector3 (1 :: GLfloat) 0 0
    square n
  preservingMatrix $ do
    coloredFace (1.0, 0.0, 1.0)
    translate $ Vector3 0 (n / 2) 0
    rotate 90 $ Vector3 (1 :: GLfloat) 0 0
    square n

newtype State = State
  { sPos :: IORef (Double, Double, Double)
  }

calculatePointOfView :: Double -> Double -> Double -> (Double, Double, Double)
calculatePointOfView alp bet r =
  let alpha = alp * 2 * pi / 360
      beta = bet * 2 * pi / 360
      y = r * cos alpha
      u = r * sin alpha
      x = u * cos beta
      z = u * sin beta
   in (x, y, z)

setPointOfView :: IORef (Double, Double, Double) -> IO ()
setPointOfView pPos = do
  (alpha, beta, r) <- get pPos
  let (x, y, z) = calculatePointOfView alpha beta r
      (x2, y2, z2) = calculatePointOfView (alpha + 90) beta r
  lookAt (Vertex3 x y z) (Vertex3 0 0 0) (Vector3 x2 y2 z2)

display :: State -> DisplayCallback
display State {..} = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  setPointOfView sPos
  colorCube 1
  flush
  swapBuffers

getWindowCenterPosition :: IO Position
getWindowCenterPosition = do
  (Size w h) <- get windowSize
  (Position x y) <- get windowPosition
  return $ Position (x + w `div` 2) (y + h `div` 2)

motion :: State -> IORef Bool -> MotionCallback
motion State {..} firstTimeCalledRef p@(Position x y) = do
  center@(Position cx cy) <- getWindowCenterPosition
  let dx = fromIntegral $ x - cx
      dy = fromIntegral $ y - cy
  firstTimeCalled <- get firstTimeCalledRef
  if firstTimeCalled
    then do
      pointerPosition $= center
      firstTimeCalledRef $= False
    else when (p /= center) $ do
           (alpha, beta, r) <- get sPos
           sPos $= (alpha + dy, beta - dx, r)
           pointerPosition $= center
           postRedisplay Nothing

keyboard :: KeyboardCallback
keyboard '\ESC' _ = leaveMainLoop
keyboard _ _      = return ()

reshape :: ReshapeCallback
reshape newScreenSize@(Size w h) = do
  viewport $= (Position 0 0, newScreenSize)
  matrixMode $= Projection
  loadIdentity
  let near = 0.001
      far = 40
      fov = 90
      ang = (fov * pi) / 360
      top = near / (cos ang / sin ang)
      aspect = fromIntegral w / fromIntegral h
      right = top * aspect
  frustum (-right) right (-top) top near far
  matrixMode $= Modelview 0

main :: IO ()
main = do
  (progName, _) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered, WithSamplesPerPixel 4]
  _ <- createWindow progName
  lighting $= Enabled
  normalize $= Enabled
  depthFunc $= Just Less
  position (Light 0) $= Vertex4 0 0 10 0
  ambient (Light 0) $= Color4 1 1 1 1
  diffuse (Light 0) $= Color4 1 1 1 1
  specular (Light 0) $= Color4 1 1 1 1
  light (Light 0) $= Enabled
  fullScreen
  cursor $= None
  state <- State <$> newIORef (90.0, 270.0, 2.0)
  displayCallback $= display state
  firstTimeCalled <- newIORef True -- need to prevent initial call to motion
  passiveMotionCallback $= Just (motion state firstTimeCalled)
  keyboardCallback $= Just keyboard
  reshapeCallback $= Just reshape
  mainLoop
