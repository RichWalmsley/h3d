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

--- Functions ---

-- | World-to-view transform: undo the camera's translation and rotation.
viewMatrix :: Camera -> Mat4
viewMatrix cam = rotation `mat4xmat4` translation
    where
        Vec3 px py pz = position cam
        Vec3 pitch yaw roll = orientation cam
        translation = translateMat4 (Vec3 (-px) (-py) (-pz)) identityMat4
        rotation =
            rotateMat4 (-roll)  (Vec3 0 0 1) $
            rotateMat4 (-pitch) (Vec3 1 0 0) $
            rotateMat4 (-yaw)   (Vec3 0 1 0) identityMat4

-- | View-to-clip perspective projection for this camera.
projectionMatrix :: Camera -> Mat4
projectionMatrix cam =
    projectMat4 (fov cam) (aspect cam) (near cam) (far cam) identityMat4