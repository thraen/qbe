module Display (display, init__) where

import Cube
import Graphics.UI.GLUT hiding (translate, scale, rotate)
import Control.Monad
import Data.IORef

points :: Int -> [(GLfloat,GLfloat,GLfloat)]
points n = [ (sin (2*pi*k/n'), cos (2*pi*k/n'), 0) | k <- [1..n'] ]
   where n' = fromIntegral n

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]

type Spin = (GLfloat, (GLfloat, GLfloat, GLfloat)) 

spinit :: Spin -> IO()
spinit (dv, w) = do
--     putStrLn $ "spinit " ++ show dv ++ " about " ++ show w
--     rotate dv (-w)
    rotate dv w

init__ = do
    loadIdentity
    scale 0.25

-- display spin_ = preservingMatrix $ do
display spin_ = do
    clear [ColorBuffer, DepthBuffer]
    spin <- get spin_
    -- viewing angle

--     orthonormal

    renderPrimitive Lines $ do
        vertfromVec (0, 0, 0)
        vertfromVec (snd spin)
    spinit spin


    orthonormal

    forM_ qp2 (draw_thing cube 0.1)
    swapBuffers

-- display = do 
--   clear [ColorBuffer]
--   renderPrimitive Points $
--      mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
--   flush
 
