module Display (display) where
 
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT
--import System.Random
--import Control.Applicative

import PState
import Control.Monad (zipWithM_)
 
rotx = Vector3 1.0 0.0 0.0
roty = Vector3 (0.0::GLfloat) 1.0 0.0
 
display pst = do
        --putStrLn "doing display"
        --hFlush stdout
        clear [ColorBuffer,DepthBuffer]
        loadIdentity
        PST geo cs (xdeg, ydeg) <- get pst
        --rotate 90.0 roty       
        --rotate (-xdeg-90.0) rotx
        rotate ydeg roty
        rotate xdeg rotx     
        scale (0.3::GLfloat) 0.3 0.3
        pointSize $= 4
        renderGeom geo cs
        --let cubeW = (0.5::GLfloat)
        --color $ Color3 (1.0::GLfloat) (0.0::GLfloat) (0.0::GLfloat)
        --cube cubeW
        preservingMatrix $ do
                color $ Color3 (1.0::GLfloat) (1.0::GLfloat) (1.0::GLfloat)
                cubeWireFrame 1.3    
        swapBuffers

vertify3 :: [(GLfloat,GLfloat,GLfloat)] -> IO ()
vertify3 verts = sequence_ $ map (\(a,b,c) -> vertex $ Vertex3 a b c) verts 
        
cubeVertices w = [ ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
        ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
        ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
        (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
        ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
        ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]        
        
cube w = renderPrimitive Quads $ vertify3 (cubeVertices w)

cubeWireFrame w = renderPrimitive Lines $ vertify3
  [ ( w,-w, w), ( w, w, w),  ( w, w, w), (-w, w, w),
    (-w, w, w), (-w,-w, w),  (-w,-w, w), ( w,-w, w),
    ( w,-w, w), ( w,-w,-w),  ( w, w, w), ( w, w,-w),
    (-w, w, w), (-w, w,-w),  (-w,-w, w), (-w,-w,-w),
    ( w,-w,-w), ( w, w,-w),  ( w, w,-w), (-w, w,-w),
    (-w, w,-w), (-w,-w,-w),  (-w,-w,-w), ( w,-w,-w) ]         
  
renderGeom (PState.Triangles tris) cs = zipWithM_ (\tri c -> preservingMatrix $ do
                color c
                renderPrimitive Polygon $ vertifyTri tri     
         ) tris cs
         
renderGeom (PState.Points ps) cs = renderPrimitive GL.Points $ zipWithM_ (\p (Color3 a b c) -> preservingMatrix $ do
                currentColor $= Color4 a b c 1.0
                vertex p     
         ) ps cs
         
          
renderGeom (PState.Cubes ps) cs = zipWithM_ f ps cs
        where   f::Vt -> Color3 GLfloat -> IO ()
                f (Vertex3 x y z) clr = preservingMatrix $ do
                        color $ clr
                        translate $ Vector3 x y z-- (0.0::GLfloat) 0.0 0.0
                        let cubeW = (0.08::GLfloat)
                        cube cubeW
                        color $ Color3 (1.0::GLfloat) (1.0::GLfloat) (1.0::GLfloat)
                        cubeWireFrame cubeW
  
vertifyTri :: Tri -> IO ()
vertifyTri [v1,v2,v3] = do
        vertex v1
        vertex v2
        vertex v3
vertifyTri _ = return ()       









-- testing other stuff
{- 
randomCPolygon = do
        list <- randomVertexs
        return $ ComplexPolygon [ ComplexContour list ]
        
randomVertexs :: IO [ AnnotatedVertex (Color3 GLfloat) ]       
randomVertexs = mapM (const  $ AnnotatedVertex <$> randomPoint <*> randomColor ) [1..100] 

randomPoint :: IO (Vertex3 GLdouble)        
randomPoint = random3floats f
        where f x y z = Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)

randomColor :: IO (Color3 GLfloat)        
randomColor = random3floats f
        where f x y z = Color3 (realToFrac x) (realToFrac y) (realToFrac z)
        
random3floats f = do
        x <- randomIO :: IO Float
        y <- randomIO :: IO Float
        z <- randomIO :: IO Float        
        return $ f x y z
        
-}       
        
         