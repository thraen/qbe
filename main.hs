import Data.IORef
import Cube
import Control.Monad

import Graphics.UI.GLUT hiding (translate, scale, rotate)

type Spin = (GLfloat, (GLfloat, GLfloat, GLfloat)) 

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]

    _window <- createWindow "Hello World"

    depthFunc $= Just Less -- the comparison function for depth the buffer

    normalizeWindowCoo_ <- newIORef (normalizeWindowCoo (Position 0 0)) 
    reshapeCallback $= Just (reshape normalizeWindowCoo_)

    spin_      <- newIORef (0.0, (0.0,0.0,0.0))
    lastMouse_ <- newIORef (Position 0 0)

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
--         putStrLn "mouse down"
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
--     putStrLn "idle"
    postRedisplay Nothing


-- points :: Int -> [(Float,Float,Float)]
-- points n = [ (sin (2*pi*k/n'), cos (2*pi*k/n'), 0) | k <- [1..n'] ]
--    where n' = fromIntegral n

-- myPoints :: [(Float,Float,Float)]
-- myPoints = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]

spinit :: Spin -> IO()
spinit (dv, w) = do
--     putStrLn $ "spinit " ++ show dv ++ " about " ++ show w
    rotate dv w

init__ = do
    loadIdentity
    scale 0.25

-- display spin_ = preservingMatrix $ do
display spin_ = do
    clear [ColorBuffer, DepthBuffer]
    spin <- get spin_
    -- viewing angle

    renderPrimitive Lines $ do
        vertfromVec (0, 0, 0)
        vertfromVec (snd spin)
    spinit spin

    orthonormal

    t <- elapsedTime
--     let fu = fromIntegral t
    let fu = sin (0.0008* (fromIntegral t))

--     forM_ cube_ (draw_thing cubelet 0.6 )
    forM_ cube_ (draw_thing cubelet fu )

    swapBuffers

-- display = do 
--   clear [ColorBuffer]
--   renderPrimitive Points $
--      mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
--   flush
 

