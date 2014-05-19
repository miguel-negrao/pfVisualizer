module Display (display) where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT

import PState
import Control.Monad (zipWithM_)

rotx :: Vector3 GLfloat
rotx = Vector3 (1.0::GLfloat) 0.0 0.0
roty :: Vector3 GLfloat
roty = Vector3 (0.0::GLfloat) 1.0 0.0
rotz :: Vector3 GLfloat
rotz = Vector3 (0.0::GLfloat) 0.0 1.0

{--display pst = do
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
        {-# SCC "renderGeomMe" #-}renderGeom geo cs
        --let cubeW = (0.5::GLfloat)
        --color $ Color3 (1.0::GLfloat) (0.0::GLfloat) (0.0::GLfloat)
        --cube cubeW
        let x = 1.3
        preservingMatrix $ do
        color $ Color3 (1.0::GLfloat) (1.0::GLfloat) (1.0::GLfloat)
        {-# SCC "cubeWireFrameRestMe" #-}cubeWireFrameRest x
        preservingMatrix $ do
        color $ Color3 (1.0::GLfloat) (0.0::GLfloat) (0.0::GLfloat)
        cubeWireFrameFront x
        preservingMatrix $ do
        color $ Color3 (0.0::GLfloat) (1.0::GLfloat) (0.0::GLfloat)
        cubeWireFrameUp x
        --testCube
        swapBuffers
--}

display :: HasGetter g => g PST -> IO ()
display pst = do
  a <- display1 pst
  display2 a
  displayBox
  displayEnd

displayEnd :: IO ()
displayEnd = swapBuffers

display1 :: HasGetter g => g PST -> IO (Geom, [Cl])
display1 pst = do
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
        return (geo,cs)


display2 :: (Geom, [Color3 GLfloat]) -> IO ()
display2 (geo,cs) = renderGeom geo cs
        --let cubeW = (0.5::GLfloat)
        --color $ Color3 (1.0::GLfloat) (0.0::GLfloat) (0.0::GLfloat)
        --cube cubeW

displayBox :: IO ()
displayBox = do        
        let x = 1.3
        preservingMatrix $ do
        color $ Color3 (1.0::GLfloat) (1.0::GLfloat) (1.0::GLfloat)
        {-# SCC "cubeWireFrameRestMe" #-}cubeWireFrameRest x
        preservingMatrix $ do
        color $ Color3 (1.0::GLfloat) (0.0::GLfloat) (0.0::GLfloat)
        cubeWireFrameFront x
        preservingMatrix $ do
        color $ Color3 (0.0::GLfloat) (1.0::GLfloat) (0.0::GLfloat)
        cubeWireFrameUp x
        --testCube
        

vertify3 :: [(GLfloat,GLfloat,GLfloat)] -> IO ()
vertify3 = mapM_ (\ (a, b, c) -> vertex $ Vertex3 a b c)

cubeVertices :: Num t => t -> [(t, t, t)]
cubeVertices w = [ ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
        ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
        ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
        (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
        ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
        ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]

cube :: GLfloat -> IO ()
cube w = renderPrimitive Quads $ vertify3 (cubeVertices w)

cubeWireFrameFront :: GLfloat -> IO ()
cubeWireFrameFront r = renderPrimitive Lines $ vertify3
  [ ( w,-w, w), ( w,-w,-w),  ( w, w, w), ( w, w,-w), ( w,-w, w), ( w,w, w), ( w,-w, -w), ( w, w, -w) ]
  where w = r * 1.0

cubeWireFrameUp :: GLfloat -> IO ()
cubeWireFrameUp r = renderPrimitive Lines $ vertify3
  --fmap (\(a,b) -> (a,b, w)) [ (w,w), (w,-w), (-w, -w), (-w, w) ]
  [ ( w,w, w), ( -w,w,w),  ( w, w, w), ( w, -w, w), ( -w, -w, w), ( w, -w, w), ( -w,-w, w), ( -w, w, w) ]
  where w = r * 1.0

cubeWireFrameRest :: GLfloat -> IO ()
cubeWireFrameRest w = renderPrimitive Lines $ vertify3
  [
    (-w, w, w), (-w, w,-w),  (-w,-w, w), (-w,-w,-w),
      ( w, w,-w), (-w, w,-w),
    (-w, w,-w), (-w,-w,-w),  (-w,-w,-w), ( w,-w,-w) ]

cubeWireFrame :: GLfloat -> IO ()
cubeWireFrame w = renderPrimitive Lines $ vertify3
  [ ( w,-w, w), ( w, w, w),  ( w, w, w), (-w, w, w),
    (-w, w, w), (-w,-w, w),  (-w,-w, w), ( w,-w, w),
    ( w,-w, w), ( w,-w,-w),  ( w, w, w), ( w, w,-w),
    (-w, w, w), (-w, w,-w),  (-w,-w, w), (-w,-w,-w),
    ( w,-w,-w), ( w, w,-w),  ( w, w,-w), (-w, w,-w),
    (-w, w,-w), (-w,-w,-w),  (-w,-w,-w), ( w,-w,-w) ]



renderTrigs0 :: Color a => [Vt] -> a -> IO ()
renderTrigs0 tri c = do
                      {-# SCC "rgColor" #-}color c
                      let v = {-# SCC "rgVertify" #-}vertifyTri tri
                      {-# SCC "rgRenderPrimitive" #-}renderPrimitive Polygon v
                          
renderTrigs :: Color a => [Vt] -> a -> IO ()
renderTrigs tri c = preservingMatrix $ renderTrigs0 tri c

renderGeom :: Geom -> [Color3 GLfloat] -> IO ()
renderGeom (PState.Triangles tris) cs = zipWithM_ renderTrigs  tris cs

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



{--
testCube :: IO ()
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
--}





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


