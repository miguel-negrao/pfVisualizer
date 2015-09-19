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

module POsc(OSCInstruction (..), startOSC ) where

import Control.Monad
import Control.Concurrent.STM
import Sound.OSC.FD
import Data.List.Split
import Graphics.Rendering.OpenGL hiding (set)

import PState
import Control.Lens


data OSCInstruction = NewTriangles [ Tri ] | AddTriangles [ Tri ] | NewColors [ Cl ] | NewPoints [Vt] |  NewCubes [Vt] | Quit
        deriving Show

packageFloats :: (GLfloat -> GLfloat -> GLfloat -> a) -> [GLfloat] -> [a]
packageFloats g xs =
        let vts = chunksOf 3 xs
        in fmap (\[a,b,c]-> g a b c) vts

--datum_real_err :: Datum -> Double
--datum_real_err = fromJust . datum_double

convertDListFloat :: [Datum] -> Maybe [GLfloat]
convertDListFloat xs = sequence $ fmap ( (fmap (realToFrac)). datum_float ) xs

parseOSCMessage :: Message -> Maybe OSCInstruction
parseOSCMessage (Message "/triangles" xs) = fmap ( NewTriangles . (chunksOf 3) . (packageFloats Vertex3)) $ convertDListFloat xs
parseOSCMessage (Message "/add_triangles" xs) = fmap ( AddTriangles. (chunksOf 3) . (packageFloats Vertex3) ) $ convertDListFloat xs
parseOSCMessage (Message "/colors" xs) = fmap ( NewColors .( packageFloats Color3) ) $ convertDListFloat xs
parseOSCMessage (Message "/points" xs) = fmap ( NewPoints.(packageFloats Vertex3) ) $ convertDListFloat xs
parseOSCMessage (Message "/cubes" xs) =  fmap ( NewCubes.(packageFloats Vertex3) ) $ convertDListFloat xs
parseOSCMessage (Message "/quit" _) = Just Quit
parseOSCMessage _ = Nothing

white :: Color3 GLfloat
white = Color3 (1.0::GLfloat) 1.0 1.0

whites :: [Color3 GLfloat]
whites = repeat white

processOSCTris :: PST -> [Tri] -> PST
processOSCTris pst@(PST _ oldColors _ _ _) tris = pst{ _geometry = PState.Triangles tris, _colors = oldColors++whites }
  
processOSCInstruction:: OSCInstruction -> PST -> PST
processOSCInstruction (NewTriangles tris) pst = processOSCTris pst tris
processOSCInstruction (AddTriangles tris) pst@(PST (PState.Triangles oldTris) oldColors _ _ _) = pst{ _geometry = newGeometry, _colors = newColors } where
  newGeometry = PState.Triangles (oldTris ++ tris)
  newColors =  oldColors++whites
processOSCInstruction (AddTriangles tris) pst =  processOSCTris pst tris
processOSCInstruction (NewColors cs) pst = set colors cs pst
processOSCInstruction (NewPoints ps) pst@(PST _ oldColors _ _ _) =pst{ _geometry = PState.Points ps, _colors =  oldColors++whites }
processOSCInstruction (NewCubes ps) pst@(PST _ oldColors _ _ _) = pst{ _geometry = PState.Cubes ps, _colors =  oldColors++whites }
processOSCInstruction Quit pst = pst{ _endProgram = True } 

startOSC :: TVar Bool -> TVar PST -> Int -> IO ()
startOSC oscUpdateTVar state port = do
  let
    procPacket fd = forever (recvMessages fd >>= procMsgs)
    procMsgs = mapM_ procMsg
    procMsg msg = case parseOSCMessage msg of
      Nothing -> putStrLn $ "pfVisualizer - parse failed on " ++ show msg
      Just inst -> do
         --print inst >> hFlush stdout
         atomically $ do
           modifyTVar state $ processOSCInstruction inst
           writeTVar oscUpdateTVar True
  tcpServer' port procPacket


{-
triangles  and colors should be groups of 3 triangles

/triangles 1,0,1,3,2,3

/colors 0.5,0.3,0.2,0.1,0.4,0.9

/quit
-}
