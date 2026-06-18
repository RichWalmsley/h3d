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