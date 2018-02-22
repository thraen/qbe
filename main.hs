import Graphics.UI.GLUT
import Data.IORef
import Display

type Spin = (GLfloat, (GLfloat, GLfloat, GLfloat)) 

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]

    _window <- createWindow "Hello World"

    depthFunc $= Just Less -- the comparison function for depth the buffer

    normalizeWindowCoo_ <- newIORef (normalizeWindowCoo (Position 0 0)) 
    reshapeCallback $= Just (reshape normalizeWindowCoo_)

    spin_            <- newIORef (0.0, (0.0,0.0,0.0))
    lastMouse_       <- newIORef (Position 0 0)

    keyboardMouseCallback $= Just (keyboardMouse lastMouse_ normalizeWindowCoo_ spin_)

--     passiveMotionCallback $= Just (passiveMotion lastMouse_ normalizeWindowCoo_ spin_)

    motionCallback $= Just (passiveMotion lastMouse_ normalizeWindowCoo_ spin_)

    idleCallback   $= Just (idle spin_)

    displayCallback $= display spin_
    init__
    mainLoop

type CoordinateNormalization = Position -> (GLfloat, GLfloat)

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

type Fvector  = (GLfloat, GLfloat, GLfloat)
type Fvec2    = (GLfloat, GLfloat)
type NormalMouseCoo = Fvec2

dist (x,y) (v,w) = sqrt ((x-v)^2 + (w-y)^2)

-- (-) :: Fvec2 -> Fvec2
-- (-) (x,y) (v,w) = (x-y, v-w)

spinFromMouseMotion :: NormalMouseCoo -> NormalMouseCoo -> Spin
spinFromMouseMotion (lx,ly) (x,y) = (d, v) where d = 5*dist (lx, ly) (x, y)
                                                 v = (ly-y, x-lx, 0.0)
    
updateSpinFromMouseMotion :: IORef CoordinateNormalization -> IORef Position -> Position -> IORef Spin -> IO ()
updateSpinFromMouseMotion normalize_ lastMouse_ mouse spin_ = do
        normalize <- get normalize_
        lastMouse <- get lastMouse_
        let nLastMouseCoo  = normalize lastMouse
        let nMouseCoo      = normalize mouse
--         putStrLn $ show nMouseCoo ++ show nLastMouseCoo

        spin_ $=! spinFromMouseMotion nLastMouseCoo nMouseCoo

keyboardMouse :: IORef Position -> IORef CoordinateNormalization -> IORef Spin -> KeyboardMouseCallback
keyboardMouse lastMouse_ normalize_ spin_ key Down modifier mouse = case key of
    (MouseButton LeftButton) -> do
        putStrLn "down"
--         updateSpinFromMouseMotion normalize_ lastMouse_ mouse spin_
        lastMouse_ $=! mouse
        spin_      $=! (0, (0,0,0))
        postRedisplay Nothing
    _ -> return ()

keyboardMouse _ _ spin_ (MouseButton LeftButton) Up _ _ = do
    postRedisplay Nothing
    
keyboardMouse _ _ _ _ _ _ _ = return ()

passiveMotion :: IORef Position -> IORef CoordinateNormalization -> IORef Spin -> MotionCallback
passiveMotion lastMouse_ normalize_ spin_ mouse = do
    updateSpinFromMouseMotion normalize_ lastMouse_ mouse spin_
    lastMouse_ $=! mouse
    postRedisplay Nothing

idle :: IORef Spin -> IdleCallback
idle spin_ = do
--     Spin $~! (
--     Spin $~! (+ 2)
    putStrLn "idle"
    postRedisplay Nothing


