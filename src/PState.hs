{-# LANGUAGE TemplateHaskell #-}
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

module PState where

import Control.Lens
import Graphics.Rendering.OpenGL

type Vt = Vertex3 GLfloat
type Cl = Color3 GLfloat
type Tri = [Vt]
data Geom = Triangles [Tri] | Points [Vt] | Cubes [Vt]

data PST = PST{
        _geometry:: Geom,
        _colors:: [ Cl ],
        _cameraRotation:: (GLfloat, GLfloat, GLfloat),
        _zoom:: GLfloat,
        _endProgram :: Bool
        }

makeLenses ''PST
