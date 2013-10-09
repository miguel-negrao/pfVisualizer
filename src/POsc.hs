module POsc(testOSC, listenOSC, OSCInstruction (..), processOSC, startOSC ) where

import System.IO
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Sound.OSC.FD
import Data.Maybe
import Data.List.Split
import GHC.Float
import Graphics.Rendering.OpenGL
import Data.IORef

import PState
import Display
import System.Exit (exitSuccess)
import Control.Concurrent.STM.TChan()

data OSCInstruction = NewTriangles [ Tri ] | AddTriangles [ Tri ] | NewColors [ Cl ] | NewPoints [Vt] |  NewCubes [Vt] | Quit
        deriving Show   
        
packageFloats :: (GLfloat -> GLfloat -> GLfloat -> a) -> [GLfloat] -> [a] 
packageFloats g xs = 
        let vts = splitEvery 3 xs
        in fmap (\[a,b,c]-> g a b c) vts

convertDListFloat :: [Datum] -> [GLfloat]
convertDListFloat = fmap (realToFrac . double2Float . datum_real_err)

parseOSC :: Maybe Message -> Maybe OSCInstruction
parseOSC (Just (Message "/triangles" xs) ) = Just $ NewTriangles $ splitEvery 3 $ packageFloats Vertex3 $ convertDListFloat xs
parseOSC (Just (Message "/add_triangles" xs) ) = Just $ AddTriangles $ splitEvery 3 $ packageFloats Vertex3 $ convertDListFloat xs
parseOSC (Just (Message "/colors" xs) ) = Just $ NewColors $ packageFloats Color3 $ convertDListFloat xs
parseOSC (Just (Message "/points" xs) ) = Just $ NewPoints $ packageFloats Vertex3 $ convertDListFloat xs
parseOSC (Just (Message "/cubes" xs) ) = Just $ NewCubes $ packageFloats Vertex3 $ convertDListFloat xs 
parseOSC (Just (Message "/quit" _) ) = Just Quit
parseOSC (Just _) = Nothing
parseOSC Nothing = Nothing

startOSC port = do
        oscInstrs <- atomically newTChan
        _ <- forkIO $ listenOSC oscInstrs port
        return oscInstrs 

listenOSC:: TChan OSCInstruction -> Int -> IO ()
listenOSC tlist port =  
        let 
                g x = fromMaybe (return ()) ( (atomically . writeTChan tlist) <$> parseOSC x ) -- >> print x >> (hFlush stdout)      
                f fd = forever (recvMessage fd >>= g )
                t = udpServer "127.0.0.1" port
        in withTransport t f


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
        

white = Color3 (1.0::GLfloat) 1.0 1.0
whites = repeat white

processOSCTris pst@(PST _ oldColors _) tris programState = programState $= pst{ geometry = PState.Triangles tris, colors = oldColors++whites }
        
processOSCInstruction:: OSCInstruction -> IORef PST -> IO()       
        
processOSCInstruction (NewTriangles tris) programState = do
        pst <- get programState
        processOSCTris pst tris programState
          
processOSCInstruction (AddTriangles tris) programState = do
        pst <- get programState
        case pst of
                PST (PState.Triangles oldTris) oldColors _ -> programState $= pst{ geometry = PState.Triangles (oldTris ++ tris), colors =  oldColors++whites }
                _ ->  processOSCTris pst tris programState          
        
               
processOSCInstruction (NewColors cs) programState = do
        pst <- get programState
        programState $= pst{ colors = cs }
        
processOSCInstruction (NewPoints ps) programState = do
        pst@(PST _ oldColors _) <- get programState
        programState $= pst{ geometry = PState.Points ps, colors =  oldColors++whites }

processOSCInstruction (NewCubes ps) programState = do
        pst@(PST _ oldColors _) <- get programState
        programState $= pst{ geometry = PState.Cubes ps, colors =  oldColors++whites }                    
         
processOSCInstruction Quit _ = exitSuccess

--Not working
withTimeout :: Int -> STM a -> IO (Maybe a)
withTimeout time fun = do
        mv <- atomically newEmptyTMVar
        tid <- forkIO $ do
                threadDelay time
                atomically (putTMVar mv ())
        x <- atomically (fmap Just fun `orElse` (takeTMVar mv >> return Nothing))
        killThread tid
        return x

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
