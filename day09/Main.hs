import qualified Data.Set as S

data Direction = U | D | R | L deriving (Eq, Read)
data Motion = Motion {direction :: Direction, length :: Int}
type Pos = (Int, Int)

parse :: String -> [Motion]
parse content = map (parseLine . splitAt 2) $ lines content

parseLine :: (String, String) -> Motion
parseLine (rawDir, rawLen) =
  Motion (read rawDir :: Direction) (read rawLen :: Int)

moveHead :: Pos ->[Motion] -> (Pos, [Motion])
moveHead p ((Motion _ 0):xs) = moveHead p xs
moveHead (x, y) ((Motion U n):xs) = ((x, y+1), Motion U (n-1):xs)
moveHead (x, y) ((Motion D n):xs) = ((x, y-1), Motion D (n-1):xs)
moveHead (x, y) ((Motion R n):xs) = ((x+1, y), Motion R (n-1):xs)
moveHead (x, y) ((Motion L n):xs) = ((x-1, y), Motion L (n-1):xs)
moveHead _ [] = ((0, 0), [])

moveTail :: Pos -> Pos -> Pos
moveTail pHeadF@(a, b) pTailI@(c, d)
  | dist `elem` [(0,1), (1,0), (0,0), (1,1)] = pTailI
  | dist == (2, 1) = (dx `div` 2 + c, b)
  | dist == (1, 2) = (a, dy `div` 2 + d)
  | otherwise = (dx `div` 2 + c, dy `div` 2 + d)
  where (dx, dy) = (a-c, b-d)
        dist = (abs dx, abs dy)

tailPositions :: [Pos] -> [Motion] -> S.Set Pos -> S.Set Pos
tailPositions _ [] posSet = posSet
tailPositions knots motions posSet =
  let (hNew, newMotions) = moveHead (head knots) motions
      newKnots@(newTail:_) = foldl (\all@(a:_) b -> (moveTail a b):all)
                             [hNew] (tail knots)
      newPosSet = S.insert newTail posSet
  in tailPositions (reverse newKnots) newMotions newPosSet

solve :: [Motion] -> Int -> Int
solve motions n = S.size $ tailPositions (replicate n (0, 0)) motions S.empty

main :: IO ()
main = do
  contents <- readFile "day09/input.txt"
  let motions = parse contents
      res1 = solve motions 2
      res2 = solve motions 10
  mapM_ print [res1, res2]
