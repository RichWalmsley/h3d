module Camera where

import Math

--- Types ---
data Camera = Camera
    {
        position :: Vec3,
        orientation :: Vec3, -- pitch, yaw, roll
        fov :: Float,
        aspect :: Float,
        near :: Float,
        far :: Float
    }
    deriving (Show, Eq)