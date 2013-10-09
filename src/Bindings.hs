module Bindings (display,reshape,keyboardMouse,mouseMotion) where
 
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.IO
 
import Display
import PState
 
reshape (Size w h) = viewport $= (Position 0 0,  Size minWH minWH) where
        minWH = min w h
        --padX = floor $ (w - (fromIntegral minWH) )/2
        --padY = floor $ (w - (fromIntegral minWH) )/2
        
--reshape sz@(Size w h) = do
--        let     b = fromIntegral (w `min` h) * 2
--                w' = fromIntegral w / b
--                h' = fromIntegral h / b
--        viewport $= (Position 0 0, sz)
--        matrixMode $= Projection        
--        loadIdentity
--        frustum (-w') w' (-h') h' 2 100
--        matrixMode $= Modelview 0
--        loadIdentity
--        translate (Vector3 0 0 (-4 :: GLfloat))        

keyboardMouse programState (Char 'f') Down _ _ = updateToAngle programState 90.0 (-90.0)
keyboardMouse programState (Char 'b') Down _ _ = updateToAngle programState 180.0 0.0
keyboardMouse programState (Char 'l') Down _ _ = updateToAngle programState 0.0 90.0
keyboardMouse programState (Char 'r') Down _ _ = updateToAngle programState 0.0 (-90.0)
keyboardMouse programState (Char 'u') Down _ _ = updateToAngle programState 0.0 0.0
keyboardMouse programState (Char 'd') Down _ _ = updateToAngle programState 0.0 (-90.0)
keyboardMouse programState (Char 's') Down _ _ = updateToAngle programState (-22.5) 45.0
keyboardMouse _ _ _ _ _ = return ()

updateToAngle programState angleX angleY = do
        pst <- get programState
        programState $= pst{ cameraRotation = (angleX, angleY) }
        display programState

mouseMotion programState (Position dx dy) = do
        pst <- get programState
        let 
                angx = (fromIntegral dx / 500-0.5)*360
                angy = (fromIntegral dy / 500-0.5)*360 
        programState $= pst{ cameraRotation = (angy,angx) }
        display programState
        --putStrLn $ "mouse" ++ (show (angy,angx))
        hFlush stdout