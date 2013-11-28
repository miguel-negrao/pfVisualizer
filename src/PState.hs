module PState where

import Graphics.Rendering.OpenGL

type Vt = Vertex3 GLfloat
type Cl = Color3 GLfloat
type Tri = [Vt]
data Geom = Triangles [Tri] | Points [Vt] | Cubes [Vt]

data PST = PST{
        geometry:: Geom,
        colors:: [ Cl ],
        cameraRotation:: (GLfloat, GLfloat, GLfloat)
        }


