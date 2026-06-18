module Framebuffer where

import Mesh ( Mesh )
import SDL

--- Types ---

data Colour = Colour
    {
        r :: Float,
        g :: Float,
        b :: Float,
        a :: Float -- alpha
    }

data Framebuffer = Framebuffer
    {
        width :: Int,
        height :: Int,
        pixels :: [[Colour]]
    }

--- Functions ---
createFramebuffer :: Int -> Int -> Framebuffer
createFramebuffer w h = Framebuffer w h (replicate h (replicate w (Colour 0 0 0 1)))

setPixel :: Framebuffer -> Int -> Int -> Colour -> Framebuffer
setPixel fb x y colour
    | x < 0 || x >= width fb || y < 0 || y >= height fb = fb -- Out of bounds, return unchanged
    | otherwise = fb { pixels = updatedPixels }
    where
        updatedPixels = take y (pixels fb) ++
                        [take x (pixels fb !! y) ++ [colour] ++ drop (x + 1) (pixels fb !! y)]++
                        drop (y + 1) (pixels fb)

getPixel :: Framebuffer -> Int -> Int -> Maybe Colour
getPixel fb x y
    | x < 0 || x >= width fb || y < 0 || y >= height fb = Nothing -- Out of bounds
    | otherwise = Just ((pixels fb !! y) !! x)

clearPixel :: Framebuffer -> Int -> Int -> Framebuffer
clearPixel fb x y = setPixel fb x y (Colour 0 0 0 1)

clearFramebuffer :: Framebuffer -> Colour -> Framebuffer
clearFramebuffer fb colour = fb { pixels = replicate (height fb) (replicate (width fb) colour) }

drawMesh :: Framebuffer -> Mesh -> Framebuffer
drawMesh fb mesh = undefined

drawFramebuffer :: Framebuffer -> IO ()
drawFramebuffer fb = undefined 