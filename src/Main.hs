{-# LANGUAGE TemplateHaskell, Rank2Types #-}

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

import System.Console.GetOpt --part of base
import System.Exit (exitSuccess)
import Graphics.Rendering.OpenGL hiding (set)
import Graphics.UI.GLUT hiding (set)
import Control.Concurrent.STM
import System.IO
import Data.Maybe
import Control.Monad
import Bindings
import PState
import POsc
import Control.Lens
import Control.Concurrent
import Foreign.C.Types (CFloat, CInt)

-- currently using 20% of one corei7 core 2GHz from 2011 when drawing at 10Hz

data Options = Options
               { _port           :: Int
               , _label          :: String
               , _opZoom         :: CFloat
               , _rotX           :: CFloat
               , _rotY           :: CFloat
               , _rotZ           :: CFloat
               , _winW           :: CInt
               , _winH           :: CInt
               , _displayHelp    :: Bool
               } deriving Show

makeLenses ''Options

defaultOptions :: Options
defaultOptions  = Options
    { _port           = 57300
    , _label          = ""
    , _opZoom         = 1.0
    , _rotX           = 275.0
    , _rotY           = 180
    , _rotZ           = 105
    , _winW           = 500
    , _winH           = 500
    , _displayHelp    = False
    }

f :: Read b => Lens' Options b -> String -> Options -> Options
f arg x opts = maybe opts (\y -> set arg y opts) (maybeRead x)

options :: [OptDescr (Options -> Options)]
options =
     [ Option "p"        ["port"]   (ReqArg (f port)   "PORT"   )    "TCP port",
       Option "l"        ["label"]  (ReqArg (set label)"LABEL"  )    "label",
       Option "o"        ["zoom"]   (ReqArg (f opZoom) "ZOOM"   )    "zoom factor",
       Option "x"        ["rotX"]   (ReqArg (f rotX)   "RADIANS")    "rotation around x axis",
       Option "y"        ["rotY"]   (ReqArg (f rotY)   "RADIANS")    "rotation around y axis",
       Option "z"        ["rotZ"]   (ReqArg (f rotZ)   "RADIANS")    "rotation around z axis",
       Option "w"        ["width"]  (ReqArg (f winW)   "PIXELS" )    "window width",
       Option "h"        ["height"] (ReqArg (f winH)   "PIXELS" )    "window height",
       Option ""         ["help"]   (NoArg  (set displayHelp True))   "show this help"
     ]

processArgs :: [String] -> Options
processArgs args = case getOpt RequireOrder options args of
    (flags, [],      [])     -> foldl (flip id) defaultOptions flags
    (_,     nonOpts, [])     -> error $ "unrecognized arguments: " ++ unwords nonOpts
    (_,     _,       msgs)   -> error $ concat msgs ++ usageInfo "Usage: pfVisualizer [OPTION...]" options


main :: IO ()
main = do
  (_,args) <- getArgsAndInitialize
  let
    Options _port _label _opZoom _rotX _rotY _rotZ _winW _winH _displayHelp = processArgs args
    p s = putStrLn s >> hFlush stdout
    is = map (/ 12) []--[1..12]
    v = PState.Triangles $ map (\x -> [ Vertex3 (0.1+x) 0.0 0.0, Vertex3 (0.0+x) 0.7 x, Vertex3 (0.0+x) (x-0.5) 0.0 ] ) is
    --v = PState.Points $ map (\x ->  Vertex3 (0.1+x) 0.0 0.0 ) is
    c = map (\x -> Color3 x 0.2 0.3) is
    r = (realToFrac _rotX, realToFrac _rotY, realToFrac _rotZ)
    initialState = PST  v c r (realToFrac _opZoom) False
  --print (args,o)
  --exitSuccess
  when _displayHelp $ do
    putStrLn $ usageInfo "Usage: pfVisualizer [OPTION...]" options
    exitSuccess
  state <- newTVarIO initialState
  oscUpdateTVar <- newTVarIO False
  _ <- forkIO $ startOSC oscUpdateTVar state  _port
  initialWindowSize $= Size (fromIntegral _winW) (fromIntegral _winH)
  window <- createWindow  _label
  initialDisplayCapabilities $= [ With  DisplayRGB,
        Where DisplayDepth IsAtLeast 16,
        With  DisplaySamples,
        Where DisplayStencil IsNotLessThan 2,
        With  DisplayDouble ]
--  lighting $= Enabled
--  ambient (Light 0) $= Color4 0.1 0.1 0.1 1.0
--  diffuse (Light 0) $= Color4 0.9 0.9 0.9 1.0
--  light (Light 0) $= Enabled
  reshapeCallback $= Just reshape
  depthFunc $= Just Less -- specifies comparison function for DepthBuffer
  keyboardMouseCallback $= Just (keyboardMouseWithUpdate window state)
  motionCallback $= Just (mouseMotion window state)
  displayCallback $= display state
  let checkOSCMessages = do
        --check OSC messages every 33 miliseconds = 30FPS
        threadDelay (1000 * 33)
        b <- atomically $ do
          b <- readTVar oscUpdateTVar
          when b $ writeTVar oscUpdateTVar False
          return b
        when b $ do
          (PST _ _ _ _ shouldEnd) <- atomically $ readTVar state
          --p ("idleCallback " ++ show shouldUpdate ++ " " ++ show shouldEnd)
          if shouldEnd then do
              p ("Exiting pfVisualizer listening on port: " ++ show _port)
              exitSuccess
            else -- do
            --p "got OSC message"
            postRedisplay (Just window)
  idleCallback $= Just checkOSCMessages
  p ("Started pfVisualizer listening on port: " ++ show _port)
  mainLoop

reshape :: Size -> IO ()
reshape (Size w h) = viewport $= (Position 0 0,  Size minWH minWH) where
        minWH = min w h

--just redraw continually, wastes cpu
--don't use this
timerCallBack :: Window -> IO ()
timerCallBack w = do
  postRedisplay $ Just w
  addTimerCallback 30 (timerCallBack w)

readOr:: Read a => a -> String -> a
readOr def = fromMaybe def . maybeRead

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
