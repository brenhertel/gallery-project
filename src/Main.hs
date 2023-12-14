{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Graphics.Matplotlib


import Data.List
import Data.Function
import System.Random

type Point = (Double, Double)
type Triangle = (Point, Point, Point)

data Color = R | G | B | None
  deriving (Eq, Show)
  
type PointColored = (Double, Double, Color)
type TriangleColored = (PointColored, PointColored, PointColored)

--get_point :: PointColored -> Color -> [PointColored]
get_point (x, y, c1) c2 = 
  case c1 == c2 of
    True -> [(x, y, c1)]
    False -> []

--get_red_guards :: [TriangleColored] -> Color -> [PointColored]
get_guards [] _ = []
get_guards ((pt1, pt2, pt3):tris) c1 = (get_point pt1 c1) ++ (get_point pt2 c1) ++ (get_point pt3 c1) ++ (get_guards tris c1)

--get_min_size_list :: [a] -> [a] -> [a] -> [a]
get_min_size_list l1 l2 l3
  | (length l1) < (length l2) && (length l1) < (length l3) = l1
  | (length l2) < (length l1) && (length l2) < (length l3) = l2
  | otherwise = l3

--get_smallest_guards :: [TriangleColored] -> PointColored
get_smallest_guards tris = let
  red_guards = nub (get_guards tris R)
  green_guards = nub (get_guards tris G)
  blue_guards = nub (get_guards tris B)
  in (get_min_size_list red_guards green_guards blue_guards)

--fill_colors :: TriangleColored -> TriangleColored 
fill_colors ((x1, y1, None), (x2, y2, None), (x3, y3, None)) = ((x1, y1, R), (x2, y2, G), (x3, y3, B))

fill_colors ((x1, y1, R), (x2, y2, None), (x3, y3, None)) = ((x1, y1, R), (x2, y2, G), (x3, y3, B)) 
fill_colors ((x1, y1, R), (x2, y2, G),    (x3, y3, None)) = ((x1, y1, R), (x2, y2, G), (x3, y3, B)) 
fill_colors ((x1, y1, R), (x2, y2, None), (x3, y3, B))    = ((x1, y1, R), (x2, y2, G), (x3, y3, B)) 
fill_colors ((x1, y1, R), (x2, y2, G),    (x3, y3, B))    = ((x1, y1, R), (x2, y2, G), (x3, y3, B)) 
fill_colors ((x1, y1, R), (x2, y2, B),    (x3, y3, None)) = ((x1, y1, R), (x2, y2, B), (x3, y3, G)) 
fill_colors ((x1, y1, R), (x2, y2, None), (x3, y3, G))    = ((x1, y1, R), (x2, y2, B), (x3, y3, G)) 
fill_colors ((x1, y1, R), (x2, y2, B),    (x3, y3, G))    = ((x1, y1, R), (x2, y2, B), (x3, y3, G)) 

fill_colors ((x1, y1, G), (x2, y2, None), (x3, y3, None)) = ((x1, y1, G), (x2, y2, B), (x3, y3, R)) 
fill_colors ((x1, y1, G), (x2, y2, R),    (x3, y3, None)) = ((x1, y1, G), (x2, y2, R), (x3, y3, B)) 
fill_colors ((x1, y1, G), (x2, y2, None), (x3, y3, B))    = ((x1, y1, G), (x2, y2, R), (x3, y3, B)) 
fill_colors ((x1, y1, G), (x2, y2, R),    (x3, y3, B))    = ((x1, y1, G), (x2, y2, R), (x3, y3, B)) 
fill_colors ((x1, y1, G), (x2, y2, B),    (x3, y3, None)) = ((x1, y1, G), (x2, y2, B), (x3, y3, R)) 
fill_colors ((x1, y1, G), (x2, y2, None), (x3, y3, R))    = ((x1, y1, G), (x2, y2, B), (x3, y3, R)) 
fill_colors ((x1, y1, G), (x2, y2, B),    (x3, y3, R))    = ((x1, y1, G), (x2, y2, B), (x3, y3, R)) 

fill_colors ((x1, y1, B), (x2, y2, None), (x3, y3, None)) = ((x1, y1, B), (x2, y2, G), (x3, y3, R)) 
fill_colors ((x1, y1, B), (x2, y2, G),    (x3, y3, None)) = ((x1, y1, B), (x2, y2, G), (x3, y3, R)) 
fill_colors ((x1, y1, B), (x2, y2, None), (x3, y3, R))    = ((x1, y1, B), (x2, y2, G), (x3, y3, R)) 
fill_colors ((x1, y1, B), (x2, y2, G),    (x3, y3, R))    = ((x1, y1, B), (x2, y2, G), (x3, y3, R)) 
fill_colors ((x1, y1, B), (x2, y2, R),    (x3, y3, None)) = ((x1, y1, B), (x2, y2, R), (x3, y3, G)) 
fill_colors ((x1, y1, B), (x2, y2, None), (x3, y3, G))    = ((x1, y1, B), (x2, y2, R), (x3, y3, G)) 
fill_colors ((x1, y1, B), (x2, y2, R),    (x3, y3, G))    = ((x1, y1, B), (x2, y2, R), (x3, y3, G)) 

fill_colors ((x1, y1, None), (x2, y2, R), (x3, y3, None)) = ((x1, y1, G), (x2, y2, R), (x3, y3, B)) 
fill_colors ((x1, y1, G),    (x2, y2, R), (x3, y3, None)) = ((x1, y1, G), (x2, y2, R), (x3, y3, B)) 
fill_colors ((x1, y1, None), (x2, y2, R), (x3, y3, B))    = ((x1, y1, G), (x2, y2, R), (x3, y3, B)) 
fill_colors ((x1, y1, G),    (x2, y2, R), (x3, y3, B))    = ((x1, y1, G), (x2, y2, R), (x3, y3, B)) 
fill_colors ((x1, y1, B),    (x2, y2, R), (x3, y3, None)) = ((x1, y1, B), (x2, y2, R), (x3, y3, G)) 
fill_colors ((x1, y1, None), (x2, y2, R), (x3, y3, G))    = ((x1, y1, B), (x2, y2, R), (x3, y3, G)) 
fill_colors ((x1, y1, B),    (x2, y2, R), (x3, y3, G))    = ((x1, y1, B), (x2, y2, R), (x3, y3, G)) 

fill_colors ((x1, y1, None), (x2, y2, G), (x3, y3, None)) = ((x1, y1, R), (x2, y2, G), (x3, y3, B)) 
fill_colors ((x1, y1, R),    (x2, y2, G), (x3, y3, None)) = ((x1, y1, R), (x2, y2, G), (x3, y3, B)) 
fill_colors ((x1, y1, None), (x2, y2, G), (x3, y3, B))    = ((x1, y1, R), (x2, y2, G), (x3, y3, B)) 
fill_colors ((x1, y1, R),    (x2, y2, G), (x3, y3, B))    = ((x1, y1, R), (x2, y2, G), (x3, y3, B)) 
fill_colors ((x1, y1, B),    (x2, y2, G), (x3, y3, None)) = ((x1, y1, B), (x2, y2, G), (x3, y3, R)) 
fill_colors ((x1, y1, None), (x2, y2, G), (x3, y3, R))    = ((x1, y1, B), (x2, y2, G), (x3, y3, R)) 
fill_colors ((x1, y1, B),    (x2, y2, G), (x3, y3, R))    = ((x1, y1, B), (x2, y2, G), (x3, y3, R)) 

fill_colors ((x1, y1, None), (x2, y2, B), (x3, y3, None)) = ((x1, y1, G), (x2, y2, B), (x3, y3, R)) 
fill_colors ((x1, y1, G),    (x2, y2, B), (x3, y3, None)) = ((x1, y1, G), (x2, y2, B), (x3, y3, R)) 
fill_colors ((x1, y1, None), (x2, y2, B), (x3, y3, R))    = ((x1, y1, G), (x2, y2, B), (x3, y3, R)) 
fill_colors ((x1, y1, G),    (x2, y2, B), (x3, y3, R))    = ((x1, y1, G), (x2, y2, B), (x3, y3, R)) 
fill_colors ((x1, y1, R),    (x2, y2, B), (x3, y3, None)) = ((x1, y1, R), (x2, y2, B), (x3, y3, G)) 
fill_colors ((x1, y1, None), (x2, y2, B), (x3, y3, G))    = ((x1, y1, R), (x2, y2, B), (x3, y3, G)) 
fill_colors ((x1, y1, R),    (x2, y2, B), (x3, y3, G))    = ((x1, y1, R), (x2, y2, B), (x3, y3, G)) 

fill_colors ((x1, y1, None), (x2, y2, None), (x3, y3, R)) = ((x1, y1, G), (x2, y2, B), (x3, y3, R)) 
fill_colors ((x1, y1, G),    (x2, y2, None), (x3, y3, R)) = ((x1, y1, G), (x2, y2, B), (x3, y3, R)) 
fill_colors ((x1, y1, None), (x2, y2, B),    (x3, y3, R)) = ((x1, y1, G), (x2, y2, B), (x3, y3, R)) 
fill_colors ((x1, y1, G),    (x2, y2, B),    (x3, y3, R)) = ((x1, y1, G), (x2, y2, B), (x3, y3, R)) 
fill_colors ((x1, y1, B),    (x2, y2, None), (x3, y3, R)) = ((x1, y1, B), (x2, y2, G), (x3, y3, R)) 
fill_colors ((x1, y1, None), (x2, y2, G),    (x3, y3, R)) = ((x1, y1, B), (x2, y2, G), (x3, y3, R)) 
fill_colors ((x1, y1, B),    (x2, y2, G),    (x3, y3, R)) = ((x1, y1, B), (x2, y2, G), (x3, y3, R)) 

fill_colors ((x1, y1, None), (x2, y2, None), (x3, y3, G)) = ((x1, y1, B), (x2, y2, R), (x3, y3, G)) 
fill_colors ((x1, y1, R),    (x2, y2, None), (x3, y3, G)) = ((x1, y1, R), (x2, y2, B), (x3, y3, G)) 
fill_colors ((x1, y1, None), (x2, y2, B),    (x3, y3, G)) = ((x1, y1, R), (x2, y2, B), (x3, y3, G)) 
fill_colors ((x1, y1, R),    (x2, y2, B),    (x3, y3, G)) = ((x1, y1, R), (x2, y2, B), (x3, y3, G)) 
fill_colors ((x1, y1, B),    (x2, y2, None), (x3, y3, G)) = ((x1, y1, B), (x2, y2, R), (x3, y3, G)) 
fill_colors ((x1, y1, None), (x2, y2, R),    (x3, y3, G)) = ((x1, y1, B), (x2, y2, R), (x3, y3, G)) 
fill_colors ((x1, y1, B),    (x2, y2, R),    (x3, y3, G)) = ((x1, y1, B), (x2, y2, R), (x3, y3, G)) 

fill_colors ((x1, y1, None), (x2, y2, None), (x3, y3, B)) = ((x1, y1, R), (x2, y2, B), (x3, y3, B)) 
fill_colors ((x1, y1, G),    (x2, y2, None), (x3, y3, B)) = ((x1, y1, G), (x2, y2, R), (x3, y3, B)) 
fill_colors ((x1, y1, None), (x2, y2, R),    (x3, y3, B)) = ((x1, y1, G), (x2, y2, R), (x3, y3, B)) 
fill_colors ((x1, y1, G),    (x2, y2, R),    (x3, y3, B)) = ((x1, y1, G), (x2, y2, R), (x3, y3, B)) 
fill_colors ((x1, y1, R),    (x2, y2, None), (x3, y3, B)) = ((x1, y1, R), (x2, y2, G), (x3, y3, B)) 
fill_colors ((x1, y1, None), (x2, y2, G),    (x3, y3, B)) = ((x1, y1, R), (x2, y2, G), (x3, y3, B)) 
fill_colors ((x1, y1, R),    (x2, y2, G),    (x3, y3, B)) = ((x1, y1, R), (x2, y2, G), (x3, y3, B)) 

--compare_color :: TriangleColored -> PointColored -> PointColored
compare_color ((x1, y1, c1), (x2, y2, c2), (x3, y3, c3)) (ptx, pty, ptc)
  | x1 == ptx && y1 == pty && c1 /= ptc = (ptx, pty, c1)
  | x2 == ptx && y2 == pty && c2 /= ptc = (ptx, pty, c2)
  | x3 == ptx && y3 == pty && c3 /= ptc = (ptx, pty, c3)
  | otherwise = (ptx, pty, ptc)

--color_future_tris :: TriangleColored -> [TriangleColored] -> [TriangleColored]
color_future_tris _ [] = []
color_future_tris cur_tri ((pt1, pt2, pt3):tris) = let
  new_pt1 = compare_color cur_tri pt1
  new_pt2 = compare_color cur_tri pt2
  new_pt3 = compare_color cur_tri pt3
  in [(new_pt1, new_pt2, new_pt3)] ++ (color_future_tris cur_tri tris)
  

--iterative_color :: [TriangleColored] -> [TriangleColored]
iterative_color [] =  []
iterative_color (tri:tris) =  let
    colored_cur_tri = fill_colors tri
    other_tris = color_future_tris colored_cur_tri tris
    in [colored_cur_tri] ++ iterative_color other_tris
      

--cvt_color_tris :: [Triangle] -> [TriangleColored]
cvt_color_tris [] = []
cvt_color_tris (((x1, y1), (x2, y2), (x3, y3)):tris) = [((x1, y1, None), (x2, y2, None), (x3, y3, None))] ++ cvt_color_tris tris

--color_triangles :: [Triangle] -> [TriangleColored]
color_triangles tris = let
  no_colored_triangles = cvt_color_tris (sort tris)
  in iterative_color no_colored_triangles
  
--check_intersection :: Point -> Point -> Point -> Point -> Bool
check_intersection (x0, y0) (x1, y1) (x2, y2) (x3, y3) = let
  p0 = (y3 - y2)*(x3 - x0) - (x3 - x2)*(y3 - y0)
  p1 = (y3 - y2)*(x3 - x1) - (x3 - x2)*(y3 - y1)
  p2 = (y1 - y0)*(x1 - x2) - (x1 - x0)*(y1 - y2)
  p3 = (y1 - y0)*(x1 - x3) - (x1 - x0)*(y1 - y3)
  in (p0*p1 <= 0) && (p2*p3 <= 0)

--check_sub_tri_intersection :: Point -> Point -> Point -> Point -> Bool
check_sub_tri_intersection p1 p2 p3 p4 = 
  case p1 /= p3 && p1 /= p4 && p2 /= p3 && p2 /= p4 of
    True -> check_intersection p1 p2 p3 p4
    False -> False    

-- check_tri_intersect :: [Point] -> Triangle -> Triangle
check_tri_intersect pts (p1, p2, p3) = 
  case (length pts) > 1 of
    True -> check_sub_tri_intersection (pts !! 0) (pts !! 1) p1 p2 || check_sub_tri_intersection (pts !! 0) (pts !! 1) p2 p3 || check_sub_tri_intersection (pts !! 0) (pts !! 1) p1 p3 || check_tri_intersect (tail pts) (p1, p2, p3)
    False -> False

--eliminate_intersect_tris :: [Point] -> [Triangle] -> [Triangle]
eliminate_intersect_tris [] tris = [] 
eliminate_intersect_tris pts [] = [] 
eliminate_intersect_tris pts (tri:tris) = 
  case (check_tri_intersect (copy_first_point pts) tri) of
    True -> eliminate_intersect_tris pts tris
    False -> [tri] ++ eliminate_intersect_tris pts tris

--is_convex :: Triangle -> Bool
is_convex ((x1, y1), (x2, y2), (x3, y3)) = let
  dx1 = x2 - x1
  dy1 = y2 - y1
  dx2 = x3 - x2
  dy2 = y3 - y2
  in (dx1 * dy2 - dy1 * dx2) < 0

--check_tri_convex :: [Point] -> Triangle -> Bool
check_tri_convex [] tri = True
check_tri_convex pts tri = 
  case (length pts) > 2 of
    False -> True
    True -> let
      p1 = pts !! 0
      p2 = pts !! 1
      p3 = pts !! 2
      in case (triSort (p1, p2, p3)) == (triSort tri) of
        True -> is_convex (p1, p2, p3)
        False -> check_tri_convex (tail pts) tri

--eliminate_concave_tris :: [Point] -> [Triangle] -> [Triangle]
eliminate_concave_tris [] tris = [] 
eliminate_concave_tris pts [] = [] 
eliminate_concave_tris pts (tri:tris) = 
  case (check_tri_convex (copy_first_point pts) tri) of
    True -> [tri] ++ eliminate_concave_tris pts tris
    False -> eliminate_concave_tris pts tris


--is_ccw :: Triangle -> Bool
is_ccw ((x1, y1), (x2, y2), (x3, y3)) = (x2 - x1)*(y3 - y1)-(x3 - x1)*(y2 - y1) > 0


--pointInCircumcircle :: Point -> Triangle -> Bool
pointInCircumcircle (x, y) ((x1, y1), (x2, y2), (x3, y3)) =
  case is_ccw ((x1, y1), (x2, y2), (x3, y3)) of 
    True -> let ax = x1 - x
                ay = y1 - y
                bx = x2 - x
                by = y2 - y
                cx = x3 - x
                cy = y3 - y
                g = (ax*ax + ay*ay) * (bx*cy - cx*by)
                h = (bx*bx + by*by) * (ax*cy - cx*ay)
                i = (cx*cx + cy*cy) * (ax*by - bx*ay)
            in
              g - h + i > 0
    False -> pointInCircumcircle (x, y) ((x1, y1), (x3, y3), (x2, y2))

--triSort :: Triangle -> Triangle
triSort ((x1, y1), (x2, y2), (x3, y3)) = let 
  t = [(x1, y1), (x2, y2), (x3, y3)]
  [(xs1, ys1), (xs2, ys2), (xs3, ys3)] = sort t
  in ((xs1, ys1), (xs2, ys2), (xs3, ys3))
  

--eliminate_bad_triangles :: [Triangle] -> [Triangle]
eliminate_bad_triangles [] = []
eliminate_bad_triangles (x:xs) = let
  tri_other = splitAt 3 x
  [(x1, y1), (x2, y2), (x3, y3)] = fst tri_other
  tri = ((x1, y1), (x2, y2), (x3, y3))
  other_pts = snd tri_other
  n_other = length other_pts
  check_points = [pointInCircumcircle pt tri | pt <- other_pts]
  in case True `elem` check_points of
    True -> eliminate_bad_triangles xs
    False -> [(triSort tri)] ++ eliminate_bad_triangles xs

-- Generate some random points for testing
--generateRandomPoints :: Int -> IO [Point]
generateRandomPoints n = do
  let gen1 = mkStdGen 1
  let gen2 = mkStdGen 2
  let randomPoints = take n $ zip (randomRs (0, 10) gen1) (randomRs (0, 10) gen2)
  return (nub randomPoints)
  
--generatePolygon1 :: [Point]
generatePolygon1 = [(1, 1), (2, 4), (3, 4), (6, 3), (4, 1), (2, 2)]

--generatePolygon2 :: [Point]
generatePolygon2 = [(1, 1), (2, 4), (3, 4), (6, 3), (2, 2)]

--generatePolygon3 :: [Point]
generatePolygon3 = [(1, 1), (2, 4), (3, 0.75), (4, 3.5), (5, 2), (5.25, 2.75), (6, 0)]


--take_x :: [Point] -> [Double]
take_x [] = []
take_x (xy:xys) = [fst(xy)] ++ take_x xys

--take_y :: [Point] -> [Double]
take_y [] = []
take_y (xy:xys) = [snd(xy)] ++ take_y xys

--copy_first_point :: [Point] -> [Point]
copy_first_point (x:xs) = [x] ++ xs ++ [x]

--plotLists xs ys title_str
plotLists x y str = onscreen $ plot x y @@ [o1 "m", o2 "linewidth" 5] % plot x y @@ [o1 "k.", o2 "linewidth" 20] % title str

plotPoints xy str = plotLists (copy_first_point(take_x xy)) (copy_first_point(take_y xy)) str

--tri_connections :: Point -> [Triangle] -> [Point]
tri_connections _ [] = []
tri_connections (px, py) (((x1, y1), (x2, y2), (x3, y3)):tris) = 
  case ((px == x1) && (py == y1)) || ((px == x2) && (py == y2)) || ((px == x3) && (py == y3)) of
    True -> [(px, py), (x1, y1), (px, py), (x2, y2), (px, py), (x3, y3), (px, py)] ++ (tri_connections (px, py) tris)
    False -> tri_connections (px, py) tris

--all_connections :: [Point] -> [Triangle] -> [Point]
all_connections [] _ = []
all_connections _ [] = []
all_connections (p:poly) tris = [p] ++ (tri_connections p tris) ++ (all_connections poly tris)

main :: IO ()
main = do
  -- points <- generateRandomPoints 5
  let points = generatePolygon2
  let all_permutations = permutations points
  let triangles = nub (eliminate_bad_triangles all_permutations)
  let tru_trus = eliminate_intersect_tris points triangles
  let del_tris = eliminate_concave_tris points tru_trus
  let colored_del_tris = color_triangles del_tris
  let guard_list = get_smallest_guards colored_del_tris
  putStrLn "Generated Points:"
  print points
  plotPoints points "Points"
  putStrLn "Delaunay Triangles:"
  print del_tris
  putStrLn "Colored Triangles:"
  print colored_del_tris
  putStrLn "Guards:"
  print guard_list
  let tri_plot = all_connections points del_tris
  plotPoints tri_plot "Delaunay Triangles"
  
