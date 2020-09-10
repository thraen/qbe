import Data.IORef
import Data.Maybe

import Cube
import Control.Monad

import Data.Matrix
-- import qualified Data.Vector as V (take, map, (++))

import Graphics.UI.GLUT hiding (translate, scale, rotate)

import Graphics.GLU

import System.Environment

import Data.Char (isLower, isUpper, toUpper, toLower)

-- import Graphics.Rendering.GLU.Raw (gluLookAt)

type SpinV = (Float, Float)
type SpinAx  = (MMatrix, MMatrix)

type Input = (Key, Integer)

π = pi

-- Transformations of the cube:
-- looking at the corner red yellow blue through the main diagonal, denote
--
-- r : turn right (red) front   counter clockwise
-- l : turn left (blue) front   clockwise
-- u : turn unten (white) front counter clockwise (look at the front each)

-- represent strings of transformations as character strings
r :: String -> String
r  s = s ++ "r" 
l  s = s ++ "l" 
u  s = s ++ "u" 
r' s = s ++ "R" 
l' s = s ++ "L" 
u' s = s ++ "U" 
ι  s = s ++ "i"

-- Module of function concatenation:
--  do function n times:
(⋅) 1 τ = τ
(⋅) n τ = (⋅) (n-1) τ.τ
 
--  (f ∘ g)⁻ = (f⁻ ∘ g⁻)
invc c 
    | isLower c = toUpper c
    | isUpper c = toLower c
inv τ s = reverse $ map invc (τ s)
(⁻) τ = (inv τ) -- ⁻ works postfix!

(∘) = (.)

-- ρ leaves upper half
τ₁ = (l ∘ r' ∘ u ∘ r)
ρ₁ = τ₁ ∘ (1⋅u) ∘ (τ₁⁻)  -- 7⋅ρ₁ = u'
ρ₂ = τ₁ ∘ (2⋅u) ∘ (τ₁⁻)  -- 3⋅ρ₂ = 2⋅u
ρ₃ = τ₁ ∘ ( u') ∘ (τ₁⁻)  -- 7⋅ρ₃ = u

-- σ leaves upper half
σ = r' ∘ (2⋅u') ∘ r ∘ u ∘ r' ∘ u ∘ r -- 6⋅σ = id

-- move = (1⋅σ) ∘ (1⋅ρ₃)

-- move = ι 

τ = 2⋅ρ₃ -- vertauschverdrehung von 2en
η = (2⋅u) ∘ σ   -- circles orientations only of 3 of lower cirlcle, leaves all positions 
                -- 3 ⋅ η = id

move = 3 ⋅ (2⋅u ∘ ρ₁)

animation_iterations = 3 ::Integer

parse_args :: [String] -> [Input]
parse_args ss = map (\c -> (Char c, 1)) (foldl (++) "i" ss)

main :: IO ()
main = do

    (_progName, _args) <- getArgsAndInitialize

    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    _window <- createWindow "Hello World"

    depthFunc $= Just Less -- the comparison function for depth the buffer

    normalizeWindowCoo_ <- newIORef (normalizeWindowCoo (Position 0 0)) 
    reshapeCallback $= Just (reshape normalizeWindowCoo_)

    spin_      <- newIORef $ (0,0)
    cam_       <- newIORef $ (ey, ex)
    lastMouse_ <- newIORef (Position 0 0)
    inputs_    <- newIORef [] -- :: IORef [Input]

--     arguments <- getArgs
--     inputs_    <- newIORef $ parse_args arguments
    
    inputs_    <- newIORef $ parse_args [ move "" ]
    print [move ""]

    cube_      <- newIORef (init_cube [])

    keyboardMouseCallback $= Just (keyboardMouse lastMouse_ cube_ inputs_ spin_)

--     passiveMotionCallback $= Just (passiveMotion lastMouse_ normalizeWindowCoo_ spin_)

    motionCallback $= Just (passiveMotion lastMouse_ normalizeWindowCoo_ spin_)

    idleCallback   $= Just (idle cube_ inputs_)

    displayCallback $= display spin_ cube_ cam_
    init__
    mainLoop

type CoordinateNormalization = Position -> Fvec2

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

dist (x,y) (v,w) = sqrt ((x-v)^2 + (w-y)^2)

spin_from_mouse :: Fvec2 -> Fvec2 -> SpinV -> SpinV
spin_from_mouse (lx,ly) (x,y) s = (vx, vy)
        where vy = -25.8* (y-ly)
              vx = -25.8* (x-lx)

update_spin :: IORef CoordinateNormalization -> IORef Position -> Position -> IORef SpinV -> IO ()
update_spin normalize_ lastMouse_ mouse spin_ = do
        normalize <- get normalize_
        last_mouse <- get lastMouse_
        last_spin <- get spin_

--         putStrLn $ show normal_mouse ++ show normal_lastmouse
        spin_ $=! spin_from_mouse (normalize last_mouse) (normalize mouse) last_spin



key_rotation_map :: Key -> Maybe ([Pose] -> [Pose])
key_rotation_map k
    | k == Char 'r' = Just $ qrotx_ (-π/2 / n)
    | k == Char 'R' = Just $ qrotx_ ( π/2 / n)

    | k == Char 'u' = Just $ qroty_ (-π/2 / n)
    | k == Char 'U' = Just $ qroty_ ( π/2 / n)

    | k == Char 'l' = Just $ qrotz_ (-π/2 / n)
    | k == Char 'L' = Just $ qrotz_ ( π/2 / n)

    | k == Char 'i' = Just $ init_cube
    | k == Char ' ' = Just $ init_cube
    | otherwise = Nothing
    where n = fromIntegral animation_iterations

keyboardMouse :: IORef Position -> 
                 IORef [Pose] -> 
                 IORef [Input] -> 
                 IORef SpinV -> KeyboardMouseCallback

keyboardMouse lastMouse_ cube_ inputs_ spin_ key Down modifier mouse = case key of
    MouseButton LeftButton -> do
        lastMouse_ $=! mouse
        spin_      $=! (0,0)
        postRedisplay Nothing

    key -> do 
        inputs_ $~!  (\l -> l ++ [(key, 1)])

keyboardMouse _ _ _ spin_ (MouseButton LeftButton) Up _ _ = do
    postRedisplay Nothing

keyboardMouse _ _ _ _ _ _ _ _ = return ()


passiveMotion :: IORef Position -> IORef CoordinateNormalization -> IORef SpinV -> MotionCallback
passiveMotion lastMouse_ normalize_ spin_ mouse = do
    update_spin normalize_ lastMouse_ mouse spin_
    lastMouse_ $=! mouse
    postRedisplay Nothing

-- each input has animation_iterations many animation steps.
advance_animation :: [Input] -> [Input]
advance_animation ( (k,i) : kis )
    | i == animation_iterations      = kis
    | isNothing (key_rotation_map k) = kis
    | otherwise = ( (k, i+1) : kis )
advance_animation [] = []

get_transform :: [Input] -> [Pose] -> [Pose]
get_transform []     = ident_cube
get_transform inputs = case key_rotation_map key of
        Just tf' -> tf'
        Nothing  -> ident_cube
    where key = fst $ head inputs

-- calling show on a glut character key prints 'Char key'. we just want to see 'key'
show_key :: Key -> Char
show_key key = case key of
    Char key -> key
    _ -> ' '

-- Represent an Orientation by showing the columns 
-- of the rotation matrix of a pose.
-- (dropping the 4th coordinate)
show_cols :: MMatrix -> String
show_cols m = show [ minorMatrix 4 4 m ]

lst_at indices ls = [ls!!x|x<-indices]

print_face_permutations :: [Pose] -> IO()
print_face_permutations  cube = do
    putStrLn "--" 
    mapM_ putStrLn (map (show_cols.snd) (cube))

round_deg rad = round (180*rad/π)

-- Represent an Orientation by showing the angle 
-- about which a cubelet is rotated.
-- .. a corner cubelet is always rotated
-- around a diagonal by either 0, 120, 240 degrees
print_rotation_angles :: [Pose] -> IO()
print_rotation_angles  cube = do
    putStrLn "--" 
--     print (map (rotation_angle.snd) (cube))
--     print (map (round_deg.rotation_angle.snd) (cube))
    print (map (trace3.snd) (cube))

print_cube_at_end_of_rotation :: [Input] -> [Pose] -> IO()
print_cube_at_end_of_rotation ((key, step) : inputs) cube
    | step == animation_iterations = do 
        let cube_part = lst_at lower_indices cube
        print_face_permutations cube_part
        print_rotation_angles cube_part
    | otherwise = return()
print_cube_at_end_of_rotation [] _ = return()

idle :: IORef [Pose] -> IORef [Input] -> IdleCallback
idle cube_ inputs_ = do

    inputs <- get inputs_
    cube <- get cube_

    inputs_ $=! advance_animation inputs

    -- the order matters here :_( 
    let new_cube = get_transform inputs cube

--     print_cube_at_end_of_rotation inputs new_cube

--     let todos = map fst inputs
--     putStrLn $ map show_key todos

    cube_ $=! new_cube
    postRedisplay Nothing

init__ = do
    loadIdentity
    scale 0.25
    gluLookAt 0.0 0.0 (-1.0) 0.0 0.0 0.0 0.0 1.0 0.0

mmat3 = submatrix 1 3 1 1 

(×) a b = fromList 3 1 [ 
    a₂*b₃  - a₃*b₂ ,
    a₃*b₁  - a₁*b₃ ,
    a₁*b₂  - a₂*b₁ ]
    where [a₁, a₂, a₃] = toList(a)
          [b₁, b₂, b₃] = toList(b)

(⁺) :: MMatrix -> MMatrix
(⁺) v = (transpose v)

(˙) :: Float -> MMatrix -> MMatrix
(˙) s m = scaleMatrix s m

rotate_axis :: MMatrix -> MMatrix -> Float -> MMatrix
rotate_axis v k θ =
    (scaleMatrix (cos θ) v) + (scaleMatrix (sin θ) (k×v)) + (scaleMatrix ( 1-(cos θ)) k*((transpose k)*v))


display :: IORef SpinV -> IORef [Pose] -> IORef SpinAx -> IO()
display spin_ cube_ cam_ = do
    clear [ColorBuffer, DepthBuffer]

--     t <- elapsedTime
--     let t' = 0.1 * (fromIntegral t)

    (ax1, ax2) <- get cam_


    -- angle of view
    (vx, vy) <- get spin_

    rotate vx ax1
    rotate vy ax2
    
--     let rx__ = minorMatrix 4 4 (rx (-π*vy/180))
--     let new_ax1 = rx__ * ax1

    let new_ax1 = (rotate_axis ax1 ax2 (-π*vy/180))
    let new_ax2 = (rotate_axis ax2 new_ax1 (-π*vx/180))

    line green new_ax1
    line yellow new_ax2

--     print (vx, vy)

    cam_ $=! (new_ax1, new_ax2)

    orthonormal

    cube <- get cube_
    
--     hinten
--     vorne
--     cubelet
--     forM_ cube (draw_thing cubelet 0.8 )

--     let cube_part = lst_at lower_indices cube
    let cube_part = cube

    forM_ cube_part (draw_thing cubelet 0.6 )

    swapBuffers
