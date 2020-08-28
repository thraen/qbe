import Data.IORef
import Cube
import Control.Monad

import Data.Matrix

import Graphics.UI.GLUT hiding (translate, scale, rotate)

type Spin = MMatrix

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]

    _window <- createWindow "Hello World"

    depthFunc $= Just Less -- the comparison function for depth the buffer

    normalizeWindowCoo_ <- newIORef (normalizeWindowCoo (Position 0 0)) 
    reshapeCallback $= Just (reshape normalizeWindowCoo_)

    spin_      <- newIORef $ identity 4
    lastMouse_ <- newIORef (Position 0 0)

    cube_      <- newIORef (init_cube)

    keyboardMouseCallback $= Just (keyboardMouse lastMouse_ cube_ spin_)

--     passiveMotionCallback $= Just (passiveMotion lastMouse_ normalizeWindowCoo_ spin_)

    motionCallback $= Just (passiveMotion lastMouse_ normalizeWindowCoo_ spin_)

    idleCallback   $= Just (idle spin_)

    displayCallback $= display spin_ cube_
    init__
    mainLoop

type CoordinateNormalization = Position -> (Float, Float)

normalizeWindowCoo :: Position -> CoordinateNormalization
normalizeWindowCoo (Position w h) (Position x y) = 
    ( 2*((x_+1)/w_-0.5), 2*(-(y_+1)/h_+0.5) ) where w_ = fromIntegral w
                                                    h_ = fromIntegral h
                                                    x_ = fromIntegral x
                                                    y_ = fromIntegral y

reshape :: IORef CoordinateNormalization -> ReshapeCallback
reshape normalize_ (Size w h) = do 
    normalize_ $=! normalizeWindowCoo (Position w h)
--     putStrLn $ show size
    viewport  $= (Position 0 0, Size w h)

type Fvector  = (Float, Float, Float)
type Fvec2    = (Float, Float)
type NormalMouseCoo = Fvec2

dist (x,y) (v,w) = sqrt ((x-v)^2 + (w-y)^2)

spin_from_mouse :: NormalMouseCoo -> NormalMouseCoo -> Spin
spin_from_mouse (lx,ly) (x,y) = ry_ * rx_
    -- x, y are window coordinates
    where rx_ = rx $ (y-ly) * 0.2
          ry_ = ry $ (x-lx) * 0.2


update_spin :: IORef CoordinateNormalization -> IORef Position -> Position -> IORef Spin -> IO ()
update_spin normalize_ lastMouse_ mouse spin_ = do
        normalize <- get normalize_
        lastMouse <- get lastMouse_
--         putStrLn $ show normal_mouse ++ show normal_lastmouse
        spin_ $=! spin_from_mouse (normalize lastMouse) (normalize mouse)


keyboardMouse :: IORef Position -> 
                 IORef [Pose] -> 
                 IORef Spin -> KeyboardMouseCallback

keyboardMouse lastMouse_ cube_ spin_ key Down modifier mouse = case key of
    (MouseButton LeftButton) -> do
        lastMouse_ $=! mouse
        spin_      $=! identity 4
        postRedisplay Nothing

    -- operator $~! gets the io ref, applies the function on the 
    -- right to it and updates the ref with the result
    (Char 'r') -> cube_ $~! qrotx_ (-pi/8)
    (Char 'R') -> cube_ $~! qrotx_ ( pi/8)
    (Char 'u') -> cube_ $~! qroty_ ( pi/8)
    (Char 'U') -> cube_ $~! qroty_ (-pi/8)
    (Char 'l') -> cube_ $~! qrotz_ ( pi/8)
    (Char 'L') -> cube_ $~! qrotz_ (-pi/8)
    (Char ' ') -> cube_ $=! init_cube


    _ -> return ()

keyboardMouse _ _ spin_ (MouseButton LeftButton) Up _ _ = do
    postRedisplay Nothing
    
keyboardMouse _ _ _ _ _ _ _ = return ()


passiveMotion :: IORef Position -> IORef CoordinateNormalization -> IORef Spin -> MotionCallback
passiveMotion lastMouse_ normalize_ spin_ mouse = do
    update_spin normalize_ lastMouse_ mouse spin_
    lastMouse_ $=! mouse
    postRedisplay Nothing


idle :: IORef Spin -> IdleCallback
idle spin_ = do
    postRedisplay Nothing


init__ = do
    loadIdentity
    scale 0.25


display :: IORef Spin -> IORef [Pose] -> IO()
display spin_ cube_ = do
    clear [ColorBuffer, DepthBuffer]

    -- angle of view
    spin <- get spin_
    transform spin

    orthonormal

    t <- elapsedTime
--     let fu = fromIntegral t
--     let fu = 0.8 + 0.1*sin (0.0008* (fromIntegral t))

    cube <- get cube_

--     cubelet
--     putStrLn "--"
    forM_ cube (draw_thing cubelet 0.8 )

    swapBuffers
