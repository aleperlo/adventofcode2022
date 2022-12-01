import Data.List.Split
import Data.List

parse :: String -> [Int]
parse c = map (sum . map read . lines) $ splitOn "\n\n" c

main :: IO ()
main = do
  contents <- readFile "day01/input.txt"
  let parsed = parse contents
      res1 = maximum parsed
      res2 = sum . take 3 . reverse . sort $ parsed
  mapM_ print [res1, res2]
