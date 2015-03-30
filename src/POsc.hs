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

module POsc(testOSC, listenOSC, OSCInstruction (..), processOSC, startOSC ) where

import System.IO
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Sound.OSC.FD
import Data.Maybe
import Data.List.Split
import Graphics.Rendering.OpenGL
import Data.IORef

import PState
import Display
import System.Exit (exitSuccess)

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

startOSC :: Int -> IO (TChan OSCInstruction)
startOSC port = do
        oscInstrs <- atomically newTChan
        _ <- forkIO $ listenOSC oscInstrs port
        return oscInstrs

listenOSC:: TChan OSCInstruction -> Int -> IO ()
listenOSC chan port = tcpServer' port procPacket where
  procPacket fd = forever ( recvMessages fd >>= procMsgs )
  procMsgs = mapM_ procMsg
  procMsg msg = fromMaybe (putStrLn $ "pfVisualizer - parse failed on "++show msg) ((atomically . writeTChan chan) <$> parseOSCMessage msg ) -- >> print msg >> (hFlush stdout)
  --I've moved to tcp to not have to deal with dropped packets.
  --t = udpServer "127.0.0.1" port
  --in withTransport t f

--sendOSCExiting port = withTransport t (\fd -> sendOSC fd)
--  where t = openUDP "127.0.0.1" 57110

processOSC :: TChan OSCInstruction -> IORef PST -> IO () 
processOSC oscInstrs progState = do
        --print "checking for incoming osc:"
        --hFlush stdout
        b <- atomically $ tryReadTChan oscInstrs
        let g x = do
                processOSCInstruction x progState
                --print x
                --hFlush stdout
                display progState
        maybe (return ()) g b


white :: Color3 GLfloat
white = Color3 (1.0::GLfloat) 1.0 1.0

whites :: [Color3 GLfloat]
whites = repeat white

processOSCTris :: PST -> [Tri] -> IORef PST -> IO ()
processOSCTris pst@(PST _ oldColors _ _) tris programState = programState $= pst{ geometry = PState.Triangles tris, colors = oldColors++whites }

processOSCInstruction:: OSCInstruction -> IORef PST -> IO()

processOSCInstruction (NewTriangles tris) programState = do
        pst <- get programState
        processOSCTris pst tris programState

processOSCInstruction (AddTriangles tris) programState = do
        pst <- get programState
        case pst of
                PST (PState.Triangles oldTris) oldColors _ _ -> programState $= pst{ geometry = PState.Triangles (oldTris ++ tris), colors =  oldColors++whites }
                _ ->  processOSCTris pst tris programState


processOSCInstruction (NewColors cs) programState = do
        pst <- get programState
        programState $= pst{ colors = cs }

processOSCInstruction (NewPoints ps) programState = do
        pst@(PST _ oldColors _ _) <- get programState
        programState $= pst{ geometry = PState.Points ps, colors =  oldColors++whites }

processOSCInstruction (NewCubes ps) programState = do
        pst@(PST _ oldColors _ _) <- get programState
        programState $= pst{ geometry = PState.Cubes ps, colors =  oldColors++whites }

processOSCInstruction Quit _ = exitSuccess

--Not working
{--
withTimeout :: Int -> STM a -> IO (Maybe a)
withTimeout time fun = do
        mv <- atomically newEmptyTMVar
        tid <- forkIO $ do
                threadDelay time
                atomically (putTMVar mv ())
        x <- atomically (fmap Just fun `orElse` (takeTMVar mv >> return Nothing))
        killThread tid
        return x
--}

--TESTING

testOSC :: IO b
testOSC = do
        aseq <- atomically newTChan
        print "testing osc"
        _ <- forkIO $ listenOSC aseq 57300
        loop aseq

loop :: Show a => TChan a -> IO b
loop aseq = forever $ do
        b <- atomically $ readTChan aseq
        print b
        hFlush stdout
        --print "waiting"
        --hFlush stdout



{-
triangles  and colors should be groups of 3 triangles

/triangles 1,0,1,3,2,3

/colors 0.5,0.3,0.2,0.1,0.4,0.9

/quit
-}
