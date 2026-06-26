module Framebuffer
    ( Colour(..)
    , colourToPixel
    , renderMesh
    ) where

import Data.List (sortOn)
import Data.Ord (Down(..))

import Math
import Mesh
import Camera (Camera, viewMatrix, projectionMatrix)
import Xlib

--- Types ---

data Colour = Colour
    {
        r :: Float,
        g :: Float,
        b :: Float,
        a :: Float -- alpha
    }

-- | A projected triangle ready to rasterise: average depth, fill colour and the
-- three integer screen-space corners.
data ScreenTri = ScreenTri
    { stDepth  :: Float
    , stColour :: Int
    , stP0     :: (Int, Int)
    , stP1     :: (Int, Int)
    , stP2     :: (Int, Int)
    }

--- Colour helpers ---

-- | Pack a colour into a 24-bit @0xRRGGBB@ pixel value.
colourToPixel :: Colour -> Int
colourToPixel (Colour cr cg cb _) =
    (channel cr * 0x10000) + (channel cg * 0x100) + channel cb
    where
        channel f = max 0 (min 255 (round (f * 255)))

-- | One colour per cube face (six faces, reused two-triangles-per-face).
faceColours :: [Int]
faceColours = concatMap (replicate 2)
    [ colourToPixel (Colour 0.90 0.20 0.20 1) -- front  red
    , colourToPixel (Colour 0.20 0.80 0.30 1) -- back   green
    , colourToPixel (Colour 0.25 0.45 0.95 1) -- left   blue
    , colourToPixel (Colour 0.95 0.80 0.20 1) -- right  yellow
    , colourToPixel (Colour 0.85 0.30 0.85 1) -- top    magenta
    , colourToPixel (Colour 0.25 0.80 0.85 1) -- bottom cyan
    ]

--- Rendering ---

-- | Render a mesh into the window's back buffer and present it.
--
-- The supplied 'Mat4' is the model transform applied to the mesh before the
-- camera's view and projection. Triangles are drawn back-to-front (painter's
-- algorithm), which yields a correct solid result for a convex mesh.
renderMesh :: Win -> Camera -> Mesh -> Mat4 -> IO ()
renderMesh win cam mesh model = do
    clearBuffer win 0x101018
    mapM_ fill ordered
    presentWindow win
    where
        mvp = projectionMatrix cam `mat4xmat4` (viewMatrix cam `mat4xmat4` model)
        w = winWidth win
        h = winHeight win

        projected =
            [ ScreenTri ((d0 + d1 + d2) / 3) col p0 p1 p2
            | (Triangle ta tb tc, col) <- zip (triangles mesh) (cycle faceColours)
            , Just (p0, d0) <- [project mvp w h ta]
            , Just (p1, d1) <- [project mvp w h tb]
            , Just (p2, d2) <- [project mvp w h tc]
            ]

        -- Farthest (largest depth) first so nearer faces are painted on top.
        ordered = sortOn (Down . stDepth) projected

        fill st = do
            setColour win (stColour st)
            fillTriangle win (stP0 st) (stP1 st) (stP2 st)

-- | Project a vertex through the MVP matrix into integer screen coordinates,
-- returning its normalised-device depth. 'Nothing' if it is behind the camera.
project :: Mat4 -> Int -> Int -> Vertex -> Maybe ((Int, Int), Float)
project mvp w h (Vertex (Vec3 x y z))
    | cw <= 1e-6 = Nothing
    | otherwise  = Just ((round sx, round sy), ndcz)
    where
        Vec4 cx cy cz cw = mat4xvec4 mvp (Vec4 x y z 1)
        ndcx = cx / cw
        ndcy = cy / cw
        ndcz = cz / cw
        sx = (ndcx * 0.5 + 0.5) * fromIntegral w
        sy = (1 - (ndcy * 0.5 + 0.5)) * fromIntegral h

-- | Fill a triangle by scanning horizontal spans between its edges.
fillTriangle :: Win -> (Int, Int) -> (Int, Int) -> (Int, Int) -> IO ()
fillTriangle win pa pb pc =
    mapM_ scan [y0 .. y2]
    where
        sorted = sortOn snd [pa, pb, pc]
        (x0, y0) = sorted !! 0
        (x1, y1) = sorted !! 1
        (x2, y2) = sorted !! 2

        scan y =
            let xLong  = edgeX (x0, y0) (x2, y2) y
                xShort = if y < y1
                            then edgeX (x0, y0) (x1, y1) y
                            else edgeX (x1, y1) (x2, y2) y
                xl = round (min xLong xShort)
                xr = round (max xLong xShort)
            in drawLine win xl y xr y

-- | X coordinate where the edge between two points crosses the scanline @y@.
edgeX :: (Int, Int) -> (Int, Int) -> Int -> Float
edgeX (xa, ya) (xb, yb) y
    | ya == yb  = fromIntegral xa
    | otherwise = fromIntegral xa
                + (fromIntegral xb - fromIntegral xa)
                * (fromIntegral (y - ya) / fromIntegral (yb - ya))