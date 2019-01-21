module Rectangle'
    ( Point
    , Rect(..)
    , mkRect
    , mkRectCorners
    , left
    , right
    , top
    , bottom
    )
    where

-- https://hackage.haskell.org/package/CV-0.3.7/docs/Utils-Rectangle.html
-- https://github.com/aleator/CV/issues/41

type Point = (Int, Int) 
data Rect = Rect {_x :: Int, _y :: Int, _w :: Int, _h :: Int} deriving (Eq,Show)


left :: Rect -> Int
left r =
    _x r


right :: Rect -> Int    
right  r = 
    _x r + _w r - 1


top :: Rect -> Int
top r =
    _y r


bottom :: Rect -> Int
bottom r = 
    _y r + _h r - 1


mkRect :: Point -> (Int, Int) -> Rect
mkRect (x,y) (w,h) = 
    Rect (x-negW) (y-negH) (abs w) (abs h)
        where
            negH | h<0  = abs h
                | h>=0 = 0
            negW | w<0  = abs w
                | w>=0 = 0


mkRectCorners  :: Point -> Point -> Rect
mkRectCorners (x1,y1) (x2,y2) = 
    Rect x y w h
        where
            x = min x1 x2
            y = min y1 y2
            w = abs (x1-x2)
            h = abs (y1-y2)          