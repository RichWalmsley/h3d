module Main where

import Control.Concurrent (threadDelay)

import Math
import Camera
import Mesh (cubeMesh)
import Framebuffer
import Xlib

screenWidth, screenHeight :: Int
screenWidth  = 640
screenHeight = 480

main :: IO ()
main = do
    result <- openWindow screenWidth screenHeight "Haskell 3D"
    case result of
        Nothing  -> putStrLn "Failed to open X display"
        Just win -> do
            let camera = Camera
                    { position    = Vec3 0 0 5
                    , orientation = Vec3 0 0 0
                    , fov         = pi / 3
                    , aspect      = fromIntegral screenHeight / fromIntegral screenWidth
                    , near        = 0.1
                    , far         = 100
                    }
            renderLoop win camera 0
            closeWindow win

-- | Continuously redraw the cube, advancing its rotation each frame.
renderLoop :: Win -> Camera -> Float -> IO ()
renderLoop win camera t = do
    let model =
            rotateMat4 (t * 0.7) (Vec3 1 0 0) $
            rotateMat4 t         (Vec3 0 1 0) $
            scaleMat4 (Vec3 0.75 0.75 0.75) identityMat4
    renderMesh win camera cubeMesh model
    threadDelay 16000 -- ~60 fps
    renderLoop win camera (t + 0.02)