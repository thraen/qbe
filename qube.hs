module Cube where

import Data.Matrix

import Graphics.GL
-- import Graphics.GLU

import Graphics.UI.GLUT hiding (translate, scale, rotate)

type ColorF   = Color3 Float

type Pose     = (MMatrix, MMatrix)

type MMatrix  = Data.Matrix.Matrix Float

rotation_angle :: MMatrix -> Float
rotation_angle m = acos ( ((trace m_) -1) / 2 )
    where m_ = submatrix 1 3 1 3 m

trace3 m = trace $ submatrix 1 3 1 3 m

round2 :: Float -> Float
round2 x = (fromIntegral (round $ x*1000) )/1000

round_matrix :: MMatrix -> MMatrix
round_matrix m = mapPos (\(r,c) -> round2) m

round_pose :: Pose -> Pose
round_pose (x,o) = (round_matrix x, round_matrix o)

-- rotation_axis :: MMatrix -> (Float, Float, Float)
-- rotation_axis m = (h-f, c-g, d-b)
--     where m_ = submatrix 1 1 3 3 m

ex  = fromList 4 1 [ 1, 0, 0, 0 ]
ey  = fromList 4 1 [ 0, 1, 0, 0 ]
ez  = fromList 4 1 [ 0, 0, 1, 0 ]

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

-- look_from :: MMatrix -> IO()
-- look_from m =
--     gluLookAt camx camy camz 0.0 0.0 0.0 0.0 (sin camz) 0.0
--     gluLookAt 0 0 (-1) 0 0 0 0 1 0
--     where camx = realToFrac $ m!(1,1)
--           camy = realToFrac $ m!(2,1)
--           camz = realToFrac $ m!(3,1)

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
    
line :: MMatrix -> IO()
line m = preservingMatrix $ do
        renderPrimitive Lines $ do
        color green
        glVertex3f     0        0        0.0
        glVertex3f (m!(1,1))   (m!(2,1))   (m!(3,1))

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

is_front (vec, ori) = z < 0 where z = vec!(3,1)
is_lower (vec, ori) = y < 0 where y = vec!(2,1)
is_right (vec, ori) = x > 0 where x = vec!(1,1)

rotx_right α p
    | is_right p = protx α p
    | otherwise  = p

roty_lower α p
    | is_lower p = proty α p
    | otherwise  = p

rotz_front α p
    | is_front p = protz α p
    | otherwise  = p

qrotx_ :: Float -> [Pose] -> [Pose]
qrotx_ α qs = map (round_pose.(rotx_right α)) qs
qroty_ α qs = map (round_pose.(roty_lower α)) qs
qrotz_ α qs = map (round_pose.(rotz_front α)) qs 
-- qrotx_ α qs = map (rotx_right α) qs
-- qroty_ α qs = map (roty_lower α) qs
-- qrotz_ α qs = map (rotz_front α) qs 

ident_cube p = p

upper_indices = [0,1,2,3]
lower_indices :: [Int]
lower_indices = [4,5,6,7]
front_indices = [0,1,4,5]

init_cube :: [Pose] -> [ Pose ]
init_cube _ = zip [a_, b_, c_, d_, e_, f_, g_, h_] (replicate 8 (identity 4))

