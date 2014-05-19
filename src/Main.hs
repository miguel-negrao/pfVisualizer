module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
--import Control.Concurrent
import Control.Concurrent.STM
import System.IO
import Data.Maybe

import Bindings
import PState
import POsc
import Safe

main :: IO ()
main = do
  (_,args) <- getArgsAndInitialize
  let port = fromMaybe (57300::Int) (listToMaybe args >>= (\x -> maybeRead x :: Maybe Int) )
  let labelg = fromMaybe "" $ atMay args 1
  putStrLn ("Started pf visualizer listening on port: " ++ show port)
  hFlush stdout
  oscTChan <- startOSC port
  initialWindowSize $= Size 500 500
  initialDisplayCapabilities $= [ With  DisplayRGB,
        Where DisplayDepth IsAtLeast 16,
        With  DisplaySamples,
        Where DisplayStencil IsNotLessThan 2,
        With  DisplayDouble ]
  _ <- createWindow $ "Plot Parameter Field - "++labelg
--  lighting $= Enabled
--  ambient (Light 0) $= Color4 0.1 0.1 0.1 1.0
--  diffuse (Light 0) $= Color4 0.9 0.9 0.9 1.0
--  light (Light 0) $= Enabled
  --reshapeCallback $= Just reshape
  depthFunc $= Just Less -- specifies comparison function for DepthBuffer
  state <- newIORef initialState
  keyboardMouseCallback $= Just (keyboardMouse state)
  motionCallback $= Just (mouseMotion state)
  idleCallback $= Just (display state)
  displayCallback $= display state
  timerCallBack oscTChan state
  actionOnWindowClose $= MainLoopReturns
  mainLoop

initialState :: PST
initialState = PST  v c r
        where
                is = map (/ 12) []--[1..12]
                v = PState.Triangles $ map (\x -> [ Vertex3 (0.1+x) 0.0 0.0, Vertex3 (0.0+x) 0.7 x, Vertex3 (0.0+x) (x-0.5) 0.0 ] ) is
                --v = PState.Points $ map (\x ->  Vertex3 (0.1+x) 0.0 0.0 ) is
                c = map (\x -> Color3 x 0.2 0.3) is
                r = (275.0, 180, 105)

timerCallBack :: TChan OSCInstruction -> IORef PST -> IO ()
timerCallBack oscInstrs state = do
        processOSC oscInstrs state
        addTimerCallback 30 (timerCallBack oscInstrs state)


maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads





