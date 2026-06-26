module Mesh where

import Math

--- Types ---
newtype Vertex = Vertex
    {
        position :: Vec3
    }
    deriving (Show, Eq)

data Triangle = Triangle
    {
        v0 :: Vertex,
        v1 :: Vertex,
        v2 :: Vertex
    }
    deriving (Show, Eq)

data Mesh = Mesh
    {
        vertices :: [Vertex],
        triangles :: [Triangle]
    }
    deriving (Show, Eq)

--- Meshes ---

-- | A unit cube centred at the origin, spanning @[-1, 1]@ on each axis.
--
-- The 12 triangles are grouped two-per-face in the order:
-- front, back, left, right, top, bottom.
cubeMesh :: Mesh
cubeMesh = Mesh verts tris
    where
        -- 8 corners, named by the sign of (x, y, z).
        nnn = Vertex (Vec3 (-1) (-1) (-1))
        pnn = Vertex (Vec3 ( 1) (-1) (-1))
        ppn = Vertex (Vec3 ( 1) ( 1) (-1))
        npn = Vertex (Vec3 (-1) ( 1) (-1))
        nnp = Vertex (Vec3 (-1) (-1) ( 1))
        pnp = Vertex (Vec3 ( 1) (-1) ( 1))
        ppp = Vertex (Vec3 ( 1) ( 1) ( 1))
        npp = Vertex (Vec3 (-1) ( 1) ( 1))

        verts = [nnn, pnn, ppn, npn, nnp, pnp, ppp, npp]

        tris =
            [ Triangle nnn pnn ppn, Triangle nnn ppn npn -- front  (z = -1)
            , Triangle pnp nnp npp, Triangle pnp npp ppp -- back   (z =  1)
            , Triangle nnp nnn npn, Triangle nnp npn npp -- left   (x = -1)
            , Triangle pnn pnp ppp, Triangle pnn ppp ppn -- right  (x =  1)
            , Triangle npn ppn ppp, Triangle npn ppp npp -- top    (y =  1)
            , Triangle nnp pnp pnn, Triangle nnp pnn nnn -- bottom (y = -1)
            ]