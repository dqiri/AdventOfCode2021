--Part 1
data Vector = Vector Integer Integer
  deriving Show

vAdd :: Vector -> Vector -> Vector
vAdd (Vector i0 j0) (Vector i1 j1) = (Vector (i0 + i1) (j0 + j1))

magnitudeOfVector :: Vector -> Integer
magnitudeOfVector (Vector i j) = i * j

parseDirection :: String -> Maybe Vector
parseDirection direction =
  case direction of
    'f':'o':'r':'w':'a':'r':'d':' ':n -> Just (Vector (read n) 0)
    'd':'o':'w':'n':' ':n -> Just (Vector 0 (read n))
    'u':'p':' ':n -> Just (Vector 0 (-1 * (read n)))
    _ -> Nothing

parseDirections :: [String] -> [Vector]
parseDirections xs = [x | Just x <- (map (parseDirection) xs)]

collapseDirections :: [Vector] -> Vector
collapseDirections [] = (Vector 0 0)
collapseDirections (x:xs) = collapseDirections' (Vector 0 0) x xs

collapseDirections' :: Vector -> Vector -> [Vector] -> Vector
collapseDirections' acc el [] = (vAdd acc el)
collapseDirections' acc el (x:xs) = collapseDirections' (vAdd acc el) x xs

--Part 2
collapseWithAim :: [Vector] -> Vector
collapseWithAim [] = (Vector 0 0)
collapseWithAim (x:xs) = collapseWithAim' (Vector 0 0) 0 x xs

--                  (fwd, depth) -> Aim     -> el     -> List     -> (fwd, depth)
collapseWithAim' :: Vector       -> Integer -> Vector -> [Vector] -> Vector
collapseWithAim' (Vector fwd depth) currAim (Vector newFwd newAim) [] =
  if newFwd > 0
  then Vector finalFwd finalDepth
  else Vector fwd depth
  where finalFwd = newFwd + fwd
        finalDepth = depth + newFwd * currAim
collapseWithAim' (Vector fwd depth) currAim (Vector newFwd newAim) (x:xs) =
  if newFwd > 0
  then collapseWithAim' (Vector finalFwd finalDepth) currAim x xs
  else collapseWithAim' (Vector fwd depth) finalAim x xs
  where finalFwd = newFwd + fwd
        finalDepth = depth + newFwd * currAim
        finalAim = newAim + currAim

main = let
 listOfDirections = [] -- https://adventofcode.com/2021/day/2/input
 in
   (putStrLn . show . magnitudeOfVector . collapseDirections . parseDirections) listOfDirections
   -- (putStrLn . show . magnitudeOfVector . collapseWithAim . parseDirections) listOfDirections
