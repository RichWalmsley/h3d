module Math where

--- Types ---

data Vec2 = Vec2 Float Float
    deriving (Show, Eq)
data Vec3 = Vec3 Float Float Float
    deriving (Show, Eq)
data Vec4 = Vec4 Float Float Float Float
    deriving (Show, Eq)
data Mat4 = Mat4 Vec4 Vec4 Vec4 Vec4
    deriving (Show, Eq)

--- Functions ---

-- Vec2 operations
addVec2 :: Vec2 -> Vec2 -> Vec2
addVec2 (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)

subVec2 :: Vec2 -> Vec2 -> Vec2
subVec2 (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 - x2) (y1 - y2)

mulVec2 :: Vec2 -> Float -> Vec2
mulVec2 (Vec2 x y) s = Vec2 (x * s) (y * s)

dotVec2 :: Vec2 -> Vec2 -> Float
dotVec2 (Vec2 x1 y1) (Vec2 x2 y2) = x1 * x2 + y1 * y2

normVec2 :: Vec2 -> Float
normVec2 (Vec2 x y) = sqrt (x * x + y * y)

-- Vec3 operations
addVec3 :: Vec3 -> Vec3 -> Vec3
addVec3 (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)

subVec3 :: Vec3 -> Vec3 -> Vec3
subVec3 (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)

mulVec3 :: Vec3 -> Float -> Vec3
mulVec3 (Vec3 x y z) s = Vec3 (x * s) (y * s) (z * s)

dotVec3 :: Vec3 -> Vec3 -> Float
dotVec3 (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

crossVec3 :: Vec3 -> Vec3 -> Vec3
crossVec3 (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

normalizeVec3 :: Vec3 -> Vec3
normalizeVec3 v@(Vec3 x y z)
    | magnitude == 0 = v
    | otherwise = Vec3 (x / magnitude) (y / magnitude) (z / magnitude)
    where
        magnitude = sqrt (x * x + y * y + z * z)

-- Vec4 operations
addVec4 :: Vec4 -> Vec4 -> Vec4
addVec4 (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = Vec4 (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)

subVec4 :: Vec4 -> Vec4 -> Vec4
subVec4 (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = Vec4 (x1 - x2) (y1 - y2) (z1 - z2) (w1 - w2)

mulVec4 :: Vec4 -> Float -> Vec4
mulVec4 (Vec4 x y z w) s = Vec4 (x * s) (y * s) (z * s) (w * s)

dotVec4 :: Vec4 -> Vec4 -> Float
dotVec4 (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = x1 * x2 + y1 * y2 + z1 * z2 + w1 * w2

normVec4 :: Vec4 -> Float
normVec4 (Vec4 x y z w) = sqrt (x * x + y * y + z * z + w * w)

-- Mat4 operations
identityMat4 :: Mat4
identityMat4 = Mat4 (Vec4 1 0 0 0)
                    (Vec4 0 1 0 0)
                    (Vec4 0 0 1 0)
                    (Vec4 0 0 0 1)

transposeMat4 :: Mat4 -> Mat4
transposeMat4 (Mat4 (Vec4 m00 m01 m02 m03)
                    (Vec4 m10 m11 m12 m13)
                    (Vec4 m20 m21 m22 m23)
                    (Vec4 m30 m31 m32 m33)) =
    Mat4 (Vec4 m00 m10 m20 m30)
         (Vec4 m01 m11 m21 m31)
         (Vec4 m02 m12 m22 m32)
         (Vec4 m03 m13 m23 m33)

mat4xmat4 :: Mat4 -> Mat4 -> Mat4
mat4xmat4 (Mat4 a0 a1 a2 a3) (Mat4 b0 b1 b2 b3) =
    Mat4 c0 c1 c2 c3
    where
        Mat4 c0' c1' c2' c3' = transposeMat4 (Mat4 b0 b1 b2 b3)
        c0 = Vec4 (dotVec4 a0 c0') (dotVec4 a0 c1') (dotVec4 a0 c2') (dotVec4 a0 c3')
        c1 = Vec4 (dotVec4 a1 c0') (dotVec4 a1 c1') (dotVec4 a1 c2') (dotVec4 a1 c3')
        c2 = Vec4 (dotVec4 a2 c0') (dotVec4 a2 c1') (dotVec4 a2 c2') (dotVec4 a2 c3')
        c3 = Vec4 (dotVec4 a3 c0') (dotVec4 a3 c1') (dotVec4 a3 c2') (dotVec4 a3 c3')

mat4xvec4 :: Mat4 -> Vec4 -> Vec4
mat4xvec4 (Mat4 m0 m1 m2 m3) (Vec4 x y z w) =
    Vec4 x' y' z' w'
    where
        x' = dotVec4 m0 (Vec4 x y z w)
        y' = dotVec4 m1 (Vec4 x y z w)
        z' = dotVec4 m2 (Vec4 x y z w)
        w' = dotVec4 m3 (Vec4 x y z w)

translateMat4 :: Vec3 -> Mat4 -> Mat4
translateMat4 (Vec3 x y z) =
    mat4xmat4 m'
    where
        m' = Mat4 (Vec4 1 0 0 x)
                   (Vec4 0 1 0 y)
                   (Vec4 0 0 1 z)
                   (Vec4 0 0 0 1)


rotateMat4 :: Float -> Vec3 -> Mat4 -> Mat4
rotateMat4 angle axis m =
    rotation `mat4xmat4` m
    where
        (Vec3 x y z) = normalizeVec3 axis
        c = cos angle
        s = sin angle
        t = 1 - c
        rotation = Mat4 (Vec4 (t * x * x + c) (t * x * y - s * z) (t * x * z + s * y) 0)
                        (Vec4 (t * x * y + s * z) (t * y * y + c) (t * y * z - s * x) 0)
                        (Vec4 (t * x * z - s * y) (t * y * z + s * x) (t * z * z + c) 0)
                        (Vec4 0 0 0 1)

scaleMat4 :: Vec3 -> Mat4 -> Mat4
scaleMat4 (Vec3 x y z) =
    mat4xmat4 m'
    where
        m' = Mat4 (Vec4 x 0 0 0)
                   (Vec4 0 y 0 0)
                   (Vec4 0 0 z 0)
                   (Vec4 0 0 0 1)

projectMat4 :: Float -> Float -> Float -> Float -> Mat4 -> Mat4
projectMat4 fov aspect near far =
    mat4xmat4 m'
    where
        f = 1 / tan (fov / 2)
        m' = Mat4 (Vec4 f 0 0 0)
                  (Vec4 0 (f / aspect) 0 0)
                  (Vec4 0 0 ((far + near) / (near - far)) ((2 * far * near) / (near - far)))
                  (Vec4 0 0 (-1) 0)