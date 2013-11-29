module Display (display) where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT
import System.IO
--import System.Random
--import Control.Applicative

import PState
import Control.Monad (zipWithM_)

rotx = Vector3 (1.0::GLfloat) 0.0 0.0
roty = Vector3 (0.0::GLfloat) 1.0 0.0
rotz = Vector3 (0.0::GLfloat) 0.0 1.0

display pst = do
        --putStrLn "doing display"
        --hFlush stdout
        clear [ColorBuffer,DepthBuffer]
        loadIdentity
        let scaleFactor = 0.4
        scale (scaleFactor::GLfloat) (-scaleFactor) scaleFactor
        PST geo cs (xdeg, ydeg, zdeg) <- get pst
        --print (xdeg, ydeg, zdeg)
        --hFlush stdout
        rotate xdeg rotx
        rotate ydeg roty
        rotate zdeg rotz
        --need to invert the y coordinate in scale because for some reason opengl
        --used a left-handed coordinate system, i.e., y is flipped from
        --what you would expect
        --pointSize $= 4
        renderGeom geo cs
        --let cubeW = (0.5::GLfloat)
        --color $ Color3 (1.0::GLfloat) (0.0::GLfloat) (0.0::GLfloat)
        --cube cubeW
        let x = 1.3
        preservingMatrix $ do
        color $ Color3 (1.0::GLfloat) (1.0::GLfloat) (1.0::GLfloat)
        cubeWireFrameRest x
        preservingMatrix $ do
        color $ Color3 (1.0::GLfloat) (0.0::GLfloat) (0.0::GLfloat)
        cubeWireFrameFront x
        preservingMatrix $ do
        color $ Color3 (0.0::GLfloat) (1.0::GLfloat) (0.0::GLfloat)
        cubeWireFrameUp x
        --testCube
        swapBuffers

vertify3 :: [(GLfloat,GLfloat,GLfloat)] -> IO ()
vertify3 = mapM_ (\ (a, b, c) -> vertex $ Vertex3 a b c)

cubeVertices w = [ ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
        ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
        ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
        (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
        ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
        ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]

cube w = renderPrimitive Quads $ vertify3 (cubeVertices w)

cubeWireFrameFront r = renderPrimitive Lines $ vertify3
  [ ( w,-w, w), ( w,-w,-w),  ( w, w, w), ( w, w,-w), ( w,-w, w), ( w,w, w), ( w,-w, -w), ( w, w, -w) ]
  where w = r * 1.0

cubeWireFrameUp r = renderPrimitive Lines $ vertify3
  --fmap (\(a,b) -> (a,b, w)) [ (w,w), (w,-w), (-w, -w), (-w, w) ]
  [ ( w,w, w), ( -w,w,w),  ( w, w, w), ( w, -w, w), ( -w, -w, w), ( w, -w, w), ( -w,-w, w), ( -w, w, w) ]
  where w = r * 1.0

cubeWireFrameRest w = renderPrimitive Lines $ vertify3
  [
    (-w, w, w), (-w, w,-w),  (-w,-w, w), (-w,-w,-w),
      ( w, w,-w), (-w, w,-w),
    (-w, w,-w), (-w,-w,-w),  (-w,-w,-w), ( w,-w,-w) ]

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
                        color clr
                        translate $ Vector3 x y z-- (0.0::GLfloat) 0.0 0.0
                        let cubeW = (0.06::GLfloat)
                        cube cubeW
                        let wireFrameIntensity = (0.5::GLfloat)
                        color $ Color3 wireFrameIntensity wireFrameIntensity wireFrameIntensity
                        cubeWireFrame cubeW

vertifyTri :: Tri -> IO ()
vertifyTri [v1,v2,v3] = do
        vertex v1
        vertex v2
        vertex v3
vertifyTri _ = return ()



testCube = do
        --test cube
        let xy z = [ (1.0, 1.0, z), (1.0, -1.0, z), (-1.0, -1.0, z), (-1.0, 1.0, z) ]
        let xz y = [ (1.0, y, 1.0), (1.0, y, -1.0), (-1.0, y, -1.0), (-1.0, y, 1.0) ]
        let yz y = [ (y, 1.0, 1.0), (y, 1.0, -1.0), (y, -1.0, -1.0), (y, -1.0, 1.0) ]
        --color $ Color3 (0.0::GLfloat) (1.0::GLfloat) (0.0::GLfloat)
        --renderPrimitive Quads $ vertify3 $ f (0.5)
        -- xy
        color $ Color3 (1.0::GLfloat) (0.0::GLfloat) (0.0::GLfloat)
        renderPrimitive Quads $ vertify3 $ xy (1.0)  -- up red
        color $ Color3 (0.0::GLfloat) (1.0::GLfloat) (0.0::GLfloat)
        renderPrimitive Quads $ vertify3 $ xy (-1.0) -- down green
        -- xz
        color $ Color3 (0.0::GLfloat) (0.0::GLfloat) (1.0::GLfloat)
        renderPrimitive Quads $ vertify3 $ xz (1.0) --left  blue    --left and right seem to be switched...
        color $ Color3 (1.0::GLfloat) (0.0::GLfloat) (1.0::GLfloat)
        renderPrimitive Quads $ vertify3 $ xz (-1.0) --right purple
        -- yz
        color $ Color3 (0.0::GLfloat) (1.0::GLfloat) (1.0::GLfloat)
        renderPrimitive Quads $ vertify3 $ yz (1.0) --front cyan
        color $ Color3 (1.0::GLfloat) (1.0::GLfloat) (0.0::GLfloat)
        renderPrimitive Quads $ vertify3 $ yz (-1.0) --back yellow





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


