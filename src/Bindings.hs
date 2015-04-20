{--
    (C)opyright 2013–2015 by Miguel Negrão

    This file is part of pfVisualizer.

    Foobar is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Foobar is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with pfVisualizer.  If not, see <http://www.gnu.org/licenses/>.
--}

module Bindings (display,reshape,keyboardMouse,mouseMotion,keyboardMouseWithUpdate) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Foreign.C.Types (CFloat)
import Control.Concurrent.STM

import Display
import PState


reshape :: Size -> IO ()
reshape (Size w h) = viewport $= (Position 0 0,  Size minWH minWH) where
        minWH = min w h

rotateFactor :: CFloat
rotateFactor = 5.0

keyboardMouseWithUpdate :: Window -> TVar PST -> KeyboardMouseCallback
keyboardMouseWithUpdate w programState a b c d = do
  keyboardMouse programState a b c d
  postRedisplay $ Just w

keyboardMouse :: TVar PST -> KeyboardMouseCallback
keyboardMouse programState (Char 'f') Down _ _ = updateAngle programState 270.0 180 90
keyboardMouse programState (Char 'b') Down _ _ = updateAngle programState 270.0 180.0 (-90.0)
keyboardMouse programState (Char 'l') Down _ _ = updateAngle programState 90.0 0.0 180.0
keyboardMouse programState (Char 'r') Down _ _ = updateAngle programState 90.0 0.0 0.0
keyboardMouse programState (Char 'u') Down _ _ = updateAngle programState 0.0 180.0 270.0
keyboardMouse programState (Char 'd') Down _ _ = updateAngle programState 0.0     0 (90.0)

keyboardMouse programState (Char 'p') Down _ _ = updateAngle programState 275.0 180 105
keyboardMouse programState (Char 'o') Down _ _ = updateAngle programState 275.0 180 (-75)

keyboardMouse programState (Char 'q') Down _ _ = addAngle programState (realToFrac rotateFactor) (0.0) 0.0
keyboardMouse programState (Char 'w') Down _ _ = addAngle programState (realToFrac (-rotateFactor)) (0.0) 0.0
keyboardMouse programState (Char 'a') Down _ _ = addAngle programState (0.0) (realToFrac rotateFactor) 0.0
keyboardMouse programState (Char 's') Down _ _ = addAngle programState (0.0) (realToFrac (-rotateFactor)) 0.0
keyboardMouse programState (Char 'z') Down _ _ = addAngle programState (0.0) (0.0) (realToFrac rotateFactor)
keyboardMouse programState (Char 'x') Down _ _ = addAngle programState (0.0) (0.0) (realToFrac (-rotateFactor))

keyboardMouse _ _ _ _ _ = return ()

updateAngle :: TVar PST -> GLfloat -> GLfloat -> GLfloat -> IO ()
updateAngle programState angleX angleY angleZ = do
  pst <- atomically $ readTVar programState
  atomically $ writeTVar programState pst{ cameraRotation = (angleX, angleY, angleZ) }
  

addAngle :: TVar PST -> GLfloat -> GLfloat -> GLfloat -> IO ()
addAngle programState angleX angleY angleZ = atomically $ do
        pst <- readTVar programState
        let (x,y,z) = cameraRotation pst
        writeTVar programState pst{ cameraRotation = (angleX+x, angleY+y, angleZ+z) }


mouseMotion :: Window -> TVar PST  -> Position -> IO ()
mouseMotion w s (Position dx dy) = do
        atomically $ do
          pst <- readTVar s
          let
            angx = (fromIntegral dx / 500-0.5)*360
            angy = (fromIntegral dy / 500-0.5)*360
          writeTVar s pst{ cameraRotation = (270+angy,180,90.0+angx) }
        postRedisplay $ Just w  

        -- putStrLn $ "mouse" ++ (show (dx,dy))
       -- hFlush stdout
