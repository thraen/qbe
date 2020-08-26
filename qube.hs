module Cube where

-- data MatrixComponent a => Pose a 
--     where
--     transpose

import Graphics.GL

-- import Graphics.Rendering.OpenGL.GL.VertexSpec

import Graphics.UI.GLUT hiding (translate, scale, rotate)

instance Num t => Num (t,t,t) where negate (a,b,c) = (negate a, negate b, negate c)

type Fvector     = (Float, Float, Float)
type Orientation = Fvector

type ColorF   = Color3 Float

type Pose     = (Fvector, Orientation)

ex :: Fvector
ex  = (1,0,0)
ey :: Fvector
ey  = (0,1,0)
ez :: Fvector
ez  = (0,0,1)

zero :: Fvector
zero = (0, 0, 0)

-- ox :: Fvector
-- ox  = (90, 0, 0)
-- oy :: Fvector
-- oy  = ( 0,90, 0)
-- oz :: Fvector
-- oz  = ( 0, 0,90)

base :: [ Fvector ]
base = [ ex, ey, ez ]

vertexv :: Fvector ->  IO ()
vertexv (x,y,z) = vertex $ Vertex3 x y z

translate :: Fvector -> IO()
translate (x,y,z) = glTranslatef x y z

scale :: Float -> IO()
scale s = glScalef s s s

rotate :: Float -> Fvector -> IO()
rotate a (x,y,z) = glRotatef a x y z

white ::Color3 Float
white = Color3 1 1 1

red :: Color3 Float
red = Color3 1 0 0

blue :: Color3 Float
blue = Color3 0 0 1

green :: Color3 Float
green = Color3 0 1 0

yellow :: Color3 Float
yellow = Color3 1 1 0

lila :: Color3 Float
lila = Color3 1 0.5 0

euler_rotate :: Fvector -> IO()
euler_rotate (ax, ay, az) = do
--     putStrLn $ show (ax,ay,az)
    rotate ax ex
    rotate ay ey
    rotate az ez

-- vertfromVec :: Fvector -> Vertex3 Float
vertfromVec (a,b,c) = glVertex3f a b c

orthonormal :: IO()
orthonormal = preservingMatrix $ do
    renderPrimitive Lines $ do
        color red
        vertfromVec (0, 0, 0)
        vertfromVec ey
        color blue
        vertfromVec (0, 0, 0)
        vertfromVec ex
        color white
        vertfromVec (0, 0, 0)
        vertfromVec ez
--         vertex $ Vertex3 1 2 (3::Float)
--         vertex $ Vertex3 5 6 7


draw_thing :: IO() -> Float -> Pose -> IO()
draw_thing thing scl (koo, ori) = preservingMatrix $ do
    translate koo
    scale scl
--     putStrLn $ show ori
--     putStrLn $ show koo
    putStrLn $ " -- \n" ++ show (koo, ori)
    euler_rotate ori
    thing

face :: Float -> ColorF -> IO ()
face w col = renderPrimitive Quads $ do
    color col
    glVertex3f (-w) (-w) 0
    glVertex3f (-w) ( w) 0
    glVertex3f ( w) ( w) 0
    glVertex3f ( w) (-w) 0

vorne = preservingMatrix $ do
    translate ez
    face 1.0 green

hinten = preservingMatrix $ do
    translate $ -ez
    face 1.0 blue

rechts = preservingMatrix $ do
    translate ex
    rotate 90 ey
    face 1.0 red

links = preservingMatrix $ do
    translate $ -ex
    rotate 90 ey
    face 1.0 lila

unten = preservingMatrix $ do
    translate (-ey)
    rotate 90 ex
    face 1.0 white

oben = preservingMatrix $ do
    translate ey
    rotate 90 ex
    face 1.0 yellow

cubelet = preservingMatrix $ do
    vorne
    hinten
    rechts
    links
    oben
    unten

-- oberer ring
a_=(-1, 1,-1)
b_=( 1, 1,-1)
c_=( 1, 1, 1)
d_=(-1, 1, 1)
-- unterer ring
e_=(-1,-1,-1)
f_=( 1,-1,-1)
g_=( 1,-1, 1)
h_=(-1,-1, 1)


rotz :: Float -> Fvector -> Fvector
rotz α (x,y,z) = (x', y', z')
    where   x' = ( cos α)*x + (-sin α)*y
            y' = ( sin α)*x + ( cos α)*y
            z' = z

roty :: Float -> Fvector -> Fvector
roty α (x,y,z) = (x', y', z')
    where   x' = ( cos α)*x + (-sin α)*z
            y' = y
            z' = ( sin α)*x + ( cos α)*z

rotx :: Float -> Fvector -> Fvector
rotx α (x,y,z) = (x', y', z')
    where   x' = x
            y' = ( cos α)*y + (-sin α)*z
            z' = ( sin α)*y + ( cos α)*z

protz :: Float -> Pose -> Pose
protz δ (v, (α, β, γ)) = ( rotz δ v, ( α , β , γ+ 180*δ/pi ) )

proty :: Float -> Pose -> Pose
proty δ (v, (α, β, γ)) = ( roty δ v, ( α , β+ 180*δ/pi , γ ) )

protx :: Float -> Pose -> Pose
protx δ (v, (α, β, γ)) = ( rotx δ v, ( α+ 180*δ/pi , β , γ ) )

-- is_front :: Pose -> Bool
is_front ((x,y,z), o) = z < 0
-- is_lower :: Pose -> Bool
is_lower ((x,y,z), o) = y < 0
-- is_right :: Pose -> Bool
is_right ((x,y,z), o) = x > 0

rotx_right α p
    | is_right p = protx α p
    | otherwise  = p

roty_lower α p
    | is_lower p = proty α p
    | otherwise  = p

rotz_front α p
    | is_front p = protz α p
    | otherwise  = p

qrotx_ α qs = map (rotx_right α) qs
qroty_ α qs = map (roty_lower α) qs
qrotz_ α qs = map (rotz_front α) qs 

init_cube :: [ Pose ]
init_cube = zip [a_, b_, c_, d_, e_, f_, g_, h_] (replicate 8 zero)

