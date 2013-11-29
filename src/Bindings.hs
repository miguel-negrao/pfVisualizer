module Bindings (display,reshape,keyboardMouse,mouseMotion) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.IO

import Display
import PState

reshape (Size w h) = viewport $= (Position 0 0,  Size minWH minWH) where
        minWH = min w h

rotateFactor = 5.0

keyboardMouse programState (Char 'f') Down _ _ = updateAngle programState 270.0 180 90
keyboardMouse programState (Char 'b') Down _ _ = updateAngle programState 270.0 180.0 (-90.0)
keyboardMouse programState (Char 'l') Down _ _ = updateAngle programState 90.0 0.0 180.0
keyboardMouse programState (Char 'r') Down _ _ = updateAngle programState 90.0 0.0 0.0
keyboardMouse programState (Char 'u') Down _ _ = updateAngle programState 0.0 180.0 270.0
keyboardMouse programState (Char 'd') Down _ _ = updateAngle programState 0.0     0 (90.0)

keyboardMouse programState (Char 'p') Down _ _ = updateAngle programState 275.0 180 105
keyboardMouse programState (Char 'o') Down _ _ = updateAngle programState 275.0 180 (-75)

keyboardMouse programState (Char 'q') Down _ _ = addAngle programState (rotateFactor) (0.0) 0.0
keyboardMouse programState (Char 'w') Down _ _ = addAngle programState (-rotateFactor) (0.0) 0.0
keyboardMouse programState (Char 'a') Down _ _ = addAngle programState (0.0) (rotateFactor) 0.0
keyboardMouse programState (Char 's') Down _ _ = addAngle programState (0.0) (-rotateFactor) 0.0
keyboardMouse programState (Char 'z') Down _ _ = addAngle programState (0.0) (0.0) (rotateFactor)
keyboardMouse programState (Char 'x') Down _ _ = addAngle programState (0.0) (0.0) (-rotateFactor)

keyboardMouse _ _ _ _ _ = return ()

updateAngle programState angleX angleY angleZ = do
        pst <- get programState
        programState $= pst{ cameraRotation = (angleX, angleY, angleZ) }


addAngle programState angleX angleY angleZ = do
        pst <- get programState
        let (x,y,z) = cameraRotation pst
        programState $= pst{ cameraRotation = (angleX+x, angleY+y, angleZ+z) }


mouseMotion programState (Position dx dy) = do
        pst <- get programState
        let
                angx = (fromIntegral dx / 500-0.5)*360
                angy = (fromIntegral dy / 500-0.5)*360
        programState $= pst{ cameraRotation = (270+angy,180,90.0+angx) }
       -- putStrLn $ "mousehahah" ++ (show (dx,dy))
       -- hFlush stdout
