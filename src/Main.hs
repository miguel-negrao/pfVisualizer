{--
    (C)opyright 2013–2015 by Miguel Negrão

    This file is part of pfVisualizer.

    pfVisualizer is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    pfVisualizer is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with pfVisualizer.  If not, see <http://www.gnu.org/licenses/>.
--}

module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Concurrent.STM
import System.IO
import Data.Maybe
import Control.Monad
import Bindings
import PState
import POsc
import Safe
import Control.Concurrent

-- currently using 20% of one corei7 core 2GHz from 2011 when drawing at 10Hz

main :: IO ()
main = do
  (_,args) <- getArgsAndInitialize
  let
    port = fromMaybe (57300::Int) (listToMaybe args >>= (\x -> maybeRead x :: Maybe Int) )
    labelg = fromMaybe "" $ atMay args 1
    init_zoom = fromMaybe 0.5 $ atMay args 2 >>= maybeRead
    init_rotx = fromMaybe 275.0 $ atMay args 3 >>= maybeRead
    init_roty = fromMaybe 180 $ atMay args 4 >>= maybeRead
    init_rotz = fromMaybe 105 $ atMay args 5 >>= maybeRead
    p s = putStrLn s >> hFlush stdout
    initialState = PST  v c r init_zoom False
        where
                is = map (/ 12) []--[1..12]
                v = PState.Triangles $ map (\x -> [ Vertex3 (0.1+x) 0.0 0.0, Vertex3 (0.0+x) 0.7 x, Vertex3 (0.0+x) (x-0.5) 0.0 ] ) is
                --v = PState.Points $ map (\x ->  Vertex3 (0.1+x) 0.0 0.0 ) is
                c = map (\x -> Color3 x 0.2 0.3) is
                r = (init_rotx, init_roty, init_rotz)
  state <- newTVarIO initialState
  oscUpdateTVar <- newTVarIO False
  _ <- forkIO $ startOSC oscUpdateTVar state port
  window <- createWindow $ labelg
  initialWindowSize $= Size 500 500
  initialDisplayCapabilities $= [ With  DisplayRGB,
        Where DisplayDepth IsAtLeast 16,
        With  DisplaySamples,
        Where DisplayStencil IsNotLessThan 2,
        With  DisplayDouble ]
--  lighting $= Enabled
--  ambient (Light 0) $= Color4 0.1 0.1 0.1 1.0
--  diffuse (Light 0) $= Color4 0.9 0.9 0.9 1.0
--  light (Light 0) $= Enabled
  --reshapeCallback $= Just reshape
  depthFunc $= Just Less -- specifies comparison function for DepthBuffer
  keyboardMouseCallback $= Just (keyboardMouseWithUpdate window state)
  motionCallback $= Just (mouseMotion window state)
  displayCallback $= display state
  let checkOSCMessages = do
        threadDelay (1000 * 33)
        b <- atomically $ do
          b <- readTVar oscUpdateTVar
          when b $ writeTVar oscUpdateTVar False
          return b
        when b $ do  
          (PST _ _ _ _ shouldEnd) <- atomically $ readTVar state
          --p ("idleCallback " ++ show shouldUpdate ++ " " ++ show shouldEnd) 
          if shouldEnd then destroyWindow window else do
            --p "got OSC message"
            postRedisplay (Just window)
  idleCallback $= Just (checkOSCMessages)
  --timerCallBack window
  -- in order to compile a binary on OSX 10.10 that runs you need to
  -- comment out the next line (actionOnWindowClose). It uses freglut code
  -- not available in OSX which uses normal GLUT.
  actionOnWindowClose $= MainLoopReturns
  p ("Started pf visualizer listening on port: " ++ show port)
  mainLoop
  p ("Exiting pf visualizer listening on port: " ++ show port)

--just redraw continually, wastes cpu
timerCallBack :: Window -> IO ()
timerCallBack w = do
  postRedisplay $ Just w
  addTimerCallback 30 (timerCallBack w)


maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
