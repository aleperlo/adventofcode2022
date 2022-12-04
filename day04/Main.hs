import Data.List.Split

type Pair = ((Int, Int), (Int, Int))

parse :: String -> [Pair]
parse content = [((read x, read y), (read z, read t)) | [x, y, z, t] <- unparsedPairs]
  where unparsedPairs = map (splitOneOf "-,") $ lines content

overlapping :: Pair -> Int
overlapping ((x, y), (z, t)) =
  if x >= z && y <= t || x <= z && y >= t then 1 else 0

overlapping' :: Pair -> Int
overlapping' ((x, y), (z, t)) =
  if x >= z && x <= t || z >= x && z <= y then 1 else 0

solve :: [Pair] -> (Pair -> Int) -> Int
solve pairs overlappingFn = foldl (\x y -> overlappingFn y + x) 0 pairs

main :: IO ()
main = do
  contents <- readFile "day04/input.txt"
  let parsed = parse contents
      res1 = solve parsed overlapping
      res2 = solve parsed overlapping'
  mapM_ print [res1, res2]
