import Data.IORef
import Data.Maybe

import Cube
import Control.Monad

import Data.Matrix

import Graphics.UI.GLUT hiding (translate, scale, rotate)

type Spin = MMatrix

type Input = (Key, Integer)

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
    inputs_    <- newIORef [] -- :: IORef [Input]

    cube_      <- newIORef (init_cube [])

    keyboardMouseCallback $= Just (keyboardMouse lastMouse_ cube_ inputs_ spin_)

--     passiveMotionCallback $= Just (passiveMotion lastMouse_ normalizeWindowCoo_ spin_)

    motionCallback $= Just (passiveMotion lastMouse_ normalizeWindowCoo_ spin_)

    idleCallback   $= Just (idle cube_ inputs_)

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


animation_iterations = 10 ::Integer

key_rotation_map :: Key -> Maybe ([Pose] -> [Pose])
key_rotation_map k
    | k == Char 'r' = Just $ qrotx_ (-pi/2 / n)
    | k == Char 'R' = Just $ qrotx_ ( pi/2 / n)
    | k == Char 'u' = Just $ qroty_ ( pi/2 / n)
    | k == Char 'U' = Just $ qroty_ (-pi/2 / n)
    | k == Char 'l' = Just $ qrotz_ ( pi/2 / n)
    | k == Char 'L' = Just $ qrotz_ (-pi/2 / n)
    | k == Char ' ' = Just $ init_cube
    | otherwise = Nothing
    where n = fromIntegral animation_iterations

keyboardMouse :: IORef Position -> 
                 IORef [Pose] -> 
                 IORef [Input] -> 
                 IORef Spin -> KeyboardMouseCallback

keyboardMouse lastMouse_ cube_ inputs_ spin_ key Down modifier mouse = case key of
    MouseButton LeftButton -> do
        lastMouse_ $=! mouse
        spin_      $=! identity 4
        postRedisplay Nothing

    key -> inputs_ $~!  (\l -> l ++ [(key, 1)])


keyboardMouse _ _ _ spin_ (MouseButton LeftButton) Up _ _ = do
    postRedisplay Nothing

keyboardMouse _ _ _ _ _ _ _ _ = return ()


passiveMotion :: IORef Position -> IORef CoordinateNormalization -> IORef Spin -> MotionCallback
passiveMotion lastMouse_ normalize_ spin_ mouse = do
    update_spin normalize_ lastMouse_ mouse spin_
    lastMouse_ $=! mouse
    postRedisplay Nothing

advance_input :: [Input] -> [Input]
advance_input ( (k,i) : kis )
    | i == animation_iterations      = kis
    | isNothing (key_rotation_map k) = kis
    | otherwise = ( (k, i+1) : kis )
advance_input [] = []

get_transform :: [Input] -> [Pose] -> [Pose]
get_transform []     = ident_cube
get_transform inputs = case key_rotation_map key of
        Just tf' -> tf'
        Nothing  -> ident_cube
    where key = fst $ head inputs

idle :: IORef [Pose] -> IORef [Input] -> IdleCallback
idle cube_ inputs_ = do

    inputs <- get inputs_
    inputs_ $=! advance_input inputs

    let todos = map fst inputs
    putStrLn $ show todos

    cube_ $~! (get_transform inputs)


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

    cube <- get cube_
--     cubelet
    forM_ cube (draw_thing cubelet 0.8 )

    swapBuffers
