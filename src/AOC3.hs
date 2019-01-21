module AOC3
    ( mkClaim
    , linearAreaOverlap
    )
    where

import Rectangle' (Point, Rect(..), mkRect, mkRectCorners, left, right, top, bottom)
import Data.List (foldr1, length)

-- Graph (aka 'Fabric') origin is top left

type ClaimId = Int
data Claim = Claim {_id :: ClaimId, _rect :: Rect}

mkClaim :: ClaimId -> (Int, Int) -> (Int, Int) -> Claim
mkClaim id (xOffset, yOffset) dims =
    Claim id $ mkRect (xOffset + 1, yOffset + 1) dims


linearAreaOverlap :: [Claim] -> Int
linearAreaOverlap claims =    
    let
        rects = map _rect claims

        overlapPoints = 
            foldr
                (\ point acc -> if isTwoRectsCoverPoint rects point then acc ++ [point] else acc)
                []
                (rectPoints $ boundingRect rects)
    in
        Data.List.length overlapPoints


boundingRect :: [Rect] -> Rect       
boundingRect rs = 
    foldr1 
        ( \ r1 r2 -> 
            let
                xMin = min (left r1) (left r2)
                xMax = max (right r1) (right r2)
                yMin = min (top r1) (top r2)
                yMax = max (bottom r1) (bottom r2)
            in
                mkRectCorners (xMin, yMin) (xMax, yMax) 
        ) 
        rs


rectPoints :: Rect -> [Point] 
rectPoints r = 
   [ (x, y) | x <- [left r .. right r], y <- [top r .. bottom r] ] 
--    do
--         x <- [left r .. right r]
--         y <- [top r .. bottom r]
--         pure (x, y)


isTwoRectsCoverPoint :: [Rect] -> Point -> Bool
isTwoRectsCoverPoint rs p =    
    isRectsCoverPoint 2 rs p


isRectsCoverPoint :: Int -> [Rect] -> Point -> Bool
isRectsCoverPoint n rs p =       
    Data.List.length coverRects >= n
        where coverRects = filter (\ r -> isRectCoverPoint r p) rs


isRectCoverPoint :: Rect -> Point -> Bool      
isRectCoverPoint r (x, y) =
    (x >= left r) && (x <= right r) && (y >= top r) && (y <= bottom r)    