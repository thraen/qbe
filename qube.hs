module Cube where


-- data MatrixComponent a => Pose a 
--     where
--     transpose

import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.UI.GLUT hiding (translate, scale, rotate)

instance Num t => Num (t,t,t) where negate (a,b,c) = (negate a, negate b, negate c)

type F           = GLfloat
type Fvector     = (GLfloat, GLfloat, GLfloat)
type Orientation = Fvector

type ColorF   = Color3 GLfloat

type Pose     = (Fvector, Orientation)

ex :: Fvector
ex  = (1,0,0)
ey :: Fvector
ey  = (0,1,0)
ez :: Fvector
ez  = (0,0,1)

zero :: Fvector
zero = (0, 0, 0)

ox :: Fvector
ox  = (90, 0, 0)
oy :: Fvector
oy  = ( 0,90, 0)
oz :: Fvector
oz  = ( 0, 0,90)

can_base :: [ Fvector ]
can_base = [ ex, ey, ez ]

vertexv :: Fvector ->  IO ()
vertexv (x,y,z) = vertex $ Vertex3 x y z

translate :: Fvector -> IO()
translate (x,y,z) = glTranslatef x y z

scale :: F -> IO()
scale s = glScalef s s s

rotate :: F -> Fvector -> IO()
rotate a (x,y,z) = glRotatef a x y z

white ::Color3 GLfloat
white = Color3 1 1 1

red :: Color3 GLfloat
red = Color3 1 0 0

blue :: Color3 GLfloat
blue = Color3 0 0 1

green :: Color3 GLfloat
green = Color3 0 1 0

yellow :: Color3 GLfloat
yellow = Color3 1 1 0

lila :: Color3 GLfloat
lila = Color3 1 0 1

euler_rotate :: Fvector -> IO()
euler_rotate (ax, ay, az) = do
    putStrLn $ show (ax,ay,az)
    rotate ax ex
    rotate ay ey
    rotate az ez

-- vertfromVec :: Fvector -> Vertex3 GLfloat
vertfromVec (a,b,c) = glVertex3f a b c

orthonormal :: IO()
orthonormal = preservingMatrix $ do
    renderPrimitive Lines $ do
        vertfromVec (0, 0, 0)
        vertfromVec ey
        vertfromVec (0, 0, 0)
        vertfromVec ex
        vertfromVec (0, 0, 0)
        vertfromVec ez
--         vertex $ Vertex3 1 2 (3::GLfloat)
--         vertex $ Vertex3 5 6 7


draw_thing :: IO() -> F -> Pose -> IO()
draw_thing thing scl (koo, ori) = preservingMatrix $ do
    translate koo
    scale scl
    putStrLn $ show ori
    euler_rotate ori
    thing

face :: F -> ColorF -> IO ()
face w col = renderPrimitive Quads $ do
    color col
    glVertex3f (-w) (-w) 0
    glVertex3f (-w) ( w) 0
    glVertex3f ( w) ( w) 0
    glVertex3f ( w) (-w) 0

vorne = preservingMatrix $ do
    translate ez
    face 1.0 blue

hinten = preservingMatrix $ do
    translate $ -ez
    face 1.0 white

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
    face 1.0 green

oben = preservingMatrix $ do
    translate ey
    rotate 90 ex
    face 1.0 yellow

cube = preservingMatrix $ do
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

qp2 :: [ Pose ]
qp2 = zip [a_, b_, c_, d_, e_, f_, g_, h_] (replicate 8 zero)

--  / z
--  | y
--  - x


-- u :: Pose -> Pose
-- u (a_, p)

-- u [ a, b, c, d, e, f, g, h ] = [ a, b, c, d, e, f, g, h ]
