{-# LANGUAGE ForeignFunctionInterface #-}

-- | Barebones Xlib C FFI wrapper.
--
-- Provides just enough to open a window, draw into an off-screen back buffer
-- (for flicker-free animation), and copy that buffer to the screen.
module Xlib
    ( Display
    , Window
    , GC
    , Win(..)
    , openWindow
    , closeWindow
    , flushWindow
    , setColour
    , drawPixel
    , drawLine
    , fillRect
    , clearBuffer
    , presentWindow
    ) where

import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.Types (CInt(..), CULong(..), CUInt(..))
import Foreign.C.String (CString, withCString)

--- Raw C types ---

-- | Opaque @Display@ handle returned by @XOpenDisplay@.
data Display

-- | An X resource id (@XID@): windows, pixmaps, drawables, etc.
type Window = CULong

-- | A drawable target (window or pixmap).
type Drawable = CULong

-- | Opaque graphics context (@GC@) handle.
data GC

--- Foreign imports ---

foreign import ccall unsafe "X11/Xlib.h XOpenDisplay"
    c_XOpenDisplay :: CString -> IO (Ptr Display)

foreign import ccall unsafe "X11/Xlib.h XCloseDisplay"
    c_XCloseDisplay :: Ptr Display -> IO CInt

foreign import ccall unsafe "X11/Xlib.h XDefaultScreen"
    c_XDefaultScreen :: Ptr Display -> IO CInt

foreign import ccall unsafe "X11/Xlib.h XDefaultDepth"
    c_XDefaultDepth :: Ptr Display -> CInt -> IO CInt

foreign import ccall unsafe "X11/Xlib.h XRootWindow"
    c_XRootWindow :: Ptr Display -> CInt -> IO Window

foreign import ccall unsafe "X11/Xlib.h XBlackPixel"
    c_XBlackPixel :: Ptr Display -> CInt -> IO CULong

foreign import ccall unsafe "X11/Xlib.h XWhitePixel"
    c_XWhitePixel :: Ptr Display -> CInt -> IO CULong

foreign import ccall unsafe "X11/Xlib.h XCreateSimpleWindow"
    c_XCreateSimpleWindow
        :: Ptr Display -- display
        -> Window      -- parent
        -> CInt        -- x
        -> CInt        -- y
        -> CUInt       -- width
        -> CUInt       -- height
        -> CUInt       -- border width
        -> CULong      -- border pixel
        -> CULong      -- background pixel
        -> IO Window

foreign import ccall unsafe "X11/Xlib.h XCreatePixmap"
    c_XCreatePixmap
        :: Ptr Display -> Drawable
        -> CUInt -- width
        -> CUInt -- height
        -> CUInt -- depth
        -> IO Drawable

foreign import ccall unsafe "X11/Xlib.h XFreePixmap"
    c_XFreePixmap :: Ptr Display -> Drawable -> IO CInt

foreign import ccall unsafe "X11/Xlib.h XStoreName"
    c_XStoreName :: Ptr Display -> Window -> CString -> IO CInt

foreign import ccall unsafe "X11/Xlib.h XSelectInput"
    c_XSelectInput :: Ptr Display -> Window -> CLongMask -> IO CInt

foreign import ccall unsafe "X11/Xlib.h XMapWindow"
    c_XMapWindow :: Ptr Display -> Window -> IO CInt

foreign import ccall unsafe "X11/Xlib.h XFlush"
    c_XFlush :: Ptr Display -> IO CInt

foreign import ccall unsafe "X11/Xlib.h XDefaultGC"
    c_XDefaultGC :: Ptr Display -> CInt -> IO (Ptr GC)

foreign import ccall unsafe "X11/Xlib.h XSetForeground"
    c_XSetForeground :: Ptr Display -> Ptr GC -> CULong -> IO CInt

foreign import ccall unsafe "X11/Xlib.h XDrawPoint"
    c_XDrawPoint :: Ptr Display -> Drawable -> Ptr GC -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "X11/Xlib.h XDrawLine"
    c_XDrawLine
        :: Ptr Display -> Drawable -> Ptr GC
        -> CInt -> CInt -- x1 y1
        -> CInt -> CInt -- x2 y2
        -> IO CInt

foreign import ccall unsafe "X11/Xlib.h XFillRectangle"
    c_XFillRectangle
        :: Ptr Display -> Drawable -> Ptr GC
        -> CInt -> CInt -- x y
        -> CUInt -> CUInt -- width height
        -> IO CInt

foreign import ccall unsafe "X11/Xlib.h XCopyArea"
    c_XCopyArea
        :: Ptr Display -> Drawable -> Drawable -> Ptr GC
        -> CInt -> CInt -- src x y
        -> CUInt -> CUInt -- width height
        -> CInt -> CInt -- dest x y
        -> IO CInt

-- | Event mask bitfield (@long@). We only need ExposureMask = 1 << 15.
type CLongMask = CULong

exposureMask :: CLongMask
exposureMask = 0x8000

--- High level wrapper ---

-- | Everything needed to keep drawing to a window.
--
-- Drawing primitives target 'winBuffer' (an off-screen pixmap). Call
-- 'presentWindow' to copy the finished frame to the visible window.
data Win = Win
    { winDisplay :: Ptr Display
    , winWindow  :: Window
    , winBuffer  :: Drawable
    , winGC      :: Ptr GC
    , winWidth   :: Int
    , winHeight  :: Int
    }

-- | Open a window of the given width and height with the given title.
-- Returns 'Nothing' if the X display could not be opened.
openWindow :: Int -> Int -> String -> IO (Maybe Win)
openWindow w h title = do
    display <- c_XOpenDisplay nullPtr
    if display == nullPtr
        then return Nothing
        else do
            screen <- c_XDefaultScreen display
            root   <- c_XRootWindow display screen
            black  <- c_XBlackPixel display screen
            white  <- c_XWhitePixel display screen
            depth  <- c_XDefaultDepth display screen
            window <- c_XCreateSimpleWindow
                        display root
                        0 0
                        (fromIntegral w) (fromIntegral h)
                        1 black white
            buffer <- c_XCreatePixmap display window
                        (fromIntegral w) (fromIntegral h)
                        (fromIntegral depth)
            _ <- withCString title (c_XStoreName display window)
            _ <- c_XSelectInput display window exposureMask
            _ <- c_XMapWindow display window
            gc <- c_XDefaultGC display screen
            _ <- c_XFlush display
            return (Just (Win display window buffer gc w h))

-- | Close the window, free the back buffer and the display connection.
closeWindow :: Win -> IO ()
closeWindow win = do
    _ <- c_XFreePixmap (winDisplay win) (winBuffer win)
    _ <- c_XCloseDisplay (winDisplay win)
    return ()

-- | Flush all buffered drawing requests to the X server.
flushWindow :: Win -> IO ()
flushWindow win = do
    _ <- c_XFlush (winDisplay win)
    return ()

-- | Set the foreground colour used by subsequent draw calls.
-- The colour is a 24-bit @0xRRGGBB@ pixel value.
setColour :: Win -> Int -> IO ()
setColour win rgb = do
    _ <- c_XSetForeground (winDisplay win) (winGC win) (fromIntegral rgb)
    return ()

-- | Draw a single pixel at @(x, y)@ into the back buffer.
drawPixel :: Win -> Int -> Int -> IO ()
drawPixel win x y = do
    _ <- c_XDrawPoint (winDisplay win) (winBuffer win) (winGC win)
            (fromIntegral x) (fromIntegral y)
    return ()

-- | Draw a line from @(x1, y1)@ to @(x2, y2)@ into the back buffer.
drawLine :: Win -> Int -> Int -> Int -> Int -> IO ()
drawLine win x1 y1 x2 y2 = do
    _ <- c_XDrawLine (winDisplay win) (winBuffer win) (winGC win)
            (fromIntegral x1) (fromIntegral y1)
            (fromIntegral x2) (fromIntegral y2)
    return ()

-- | Fill a rectangle in the back buffer with the current foreground colour.
fillRect :: Win -> Int -> Int -> Int -> Int -> IO ()
fillRect win x y w h = do
    _ <- c_XFillRectangle (winDisplay win) (winBuffer win) (winGC win)
            (fromIntegral x) (fromIntegral y)
            (fromIntegral w) (fromIntegral h)
    return ()

-- | Clear the whole back buffer to the given @0xRRGGBB@ colour.
clearBuffer :: Win -> Int -> IO ()
clearBuffer win rgb = do
    setColour win rgb
    fillRect win 0 0 (winWidth win) (winHeight win)

-- | Copy the finished back buffer to the visible window and flush.
presentWindow :: Win -> IO ()
presentWindow win = do
    _ <- c_XCopyArea (winDisplay win) (winBuffer win) (winWindow win) (winGC win)
            0 0
            (fromIntegral (winWidth win)) (fromIntegral (winHeight win))
            0 0
    _ <- c_XFlush (winDisplay win)
    return ()

