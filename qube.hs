module Cube where

import Data.Matrix
-- libghc-hmatrix-dev

-- data MatrixComponent a => Pose a 
--     where
--     transpose

import Graphics.GL

-- import Graphics.Rendering.OpenGL.GL.VertexSpec

import Graphics.UI.GLUT hiding (translate, scale, rotate)


type ColorF   = Color3 Float

type Pose     = (MMatrix, MMatrix)

-- type GLMatrix = Graphics.UI.GLUT.Matrix
type MMatrix  = Data.Matrix.Matrix Float

ex  = fromList 4 1 [ 1, 0, 0, 1 ]
ey  = fromList 4 1 [ 0, 1, 0, 1 ]
ez  = fromList 4 1 [ 0, 0, 1, 1 ]

-- oberer ring
a_ = fromList 4 1 [ -1, 1,-1, 1 ]
b_ = fromList 4 1 [  1, 1,-1, 1 ]
c_ = fromList 4 1 [  1, 1, 1, 1 ]
d_ = fromList 4 1 [ -1, 1, 1, 1 ]

-- unterer ring
e_ = fromList 4 1 [ -1,-1,-1, 1 ]
f_ = fromList 4 1 [  1,-1,-1, 1 ]
g_ = fromList 4 1 [  1,-1, 1, 1 ]
h_ = fromList 4 1 [ -1,-1, 1, 1 ]



translate :: MMatrix -> IO()
translate m = glTranslatef x y z
    where [x, y, z, _ ] = toList m

scale :: Float -> IO()
scale s = glScalef s s s

rotate :: Float -> MMatrix -> IO()
rotate a m = glRotatef a x y z
    where [x, y, z, _ ] = toList m

white  = Color3 1 1 1
red    = Color3 1 0 0
blue   = Color3 0 0 1
green  = Color3 0 1 0
yellow = Color3 1 1 0
orange = Color3 1 0.5 0

transform :: MMatrix -> IO()
transform m = do
    tf <- (newMatrix RowMajor (toList m)) :: IO(GLmatrix Float)
    multMatrix tf
    
orthonormal :: IO()
orthonormal = preservingMatrix $ do
    renderPrimitive Lines $ do
        color red
        glVertex3f 0 0 0.0
        glVertex3f 0 1 0.0
        color blue
        glVertex3f 0 0 0.0
        glVertex3f 1 0 0.0
        color white
        glVertex3f 0 0 0.0
        glVertex3f 0 0 1.0


draw_thing :: IO() -> Float -> Pose -> IO()
draw_thing thing scl (koo, ori) = preservingMatrix $ do
    translate koo
    scale scl
--     putStrLn $ show ori
--     putStrLn $ show koo
    transform ori
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
    face 1.0 orange

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


rz :: Float -> MMatrix
rz α = fromList 4 4 [ ( cos α), (-sin α), 0, 0      ::Float,
                      ( sin α), ( cos α), 0, 0,
                          0   ,     0   , 1, 0, 
                          0   ,     0   , 0, 1  ]

ry :: Float -> MMatrix
ry α = fromList 4 4 [ ( cos α), 0, (-sin α), 0, 
                          0   , 1,     0   , 0      ::Float, 
                      ( sin α), 0, ( cos α), 0, 
                          0   , 0,     0   , 1  ]


rx :: Float -> MMatrix
rx α = fromList 4 4 [ 1,     0   ,     0    , 0     ::Float,
                      0, ( cos α), (-sin α) , 0, 
                      0, ( sin α), ( cos α) , 0, 
                      0,     0   ,     0    , 1  ]


protz :: Float -> Pose -> Pose
protz δ (v, m) = ( (rz δ) * v, (rz δ) * m )

proty :: Float -> Pose -> Pose
proty δ (v, m) = ( (ry δ) * v, (ry δ) * m )

protx :: Float -> Pose -> Pose 
protx δ (v, m) = ( (rx δ) * v, (rx δ) * m )

is_front (v, o) = z < 0 where z = v!(3,1)
is_lower (v, o) = y < 0 where y = v!(2,1)
is_right (v, o) = x > 0 where x = v!(1,1)

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
init_cube = zip [a_, b_, c_, d_, e_, f_, g_, h_] (replicate 8 (identity 4))

