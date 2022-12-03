import qualified Data.Set as S
import Data.Char
import Data.Foldable

splitRucksack :: Ord a => [a] -> [S.Set a]
splitRucksack xs = map S.fromList [take n xs, drop n xs]
  where n = length xs `div` 2

groupRucksacks :: Ord a => [[a]] -> [[S.Set a]]
groupRucksacks (x:y:z:rest) = map S.fromList [x, y, z] : groupRucksacks rest
groupRucksacks _ = []

commonItem :: Ord a => [S.Set a] -> a
commonItem xs = head $ S.elems $ foldl1 S.intersection xs

priority :: Char -> Int
priority c
  | c >= 'a' = 1 + ord c - ord 'a'
  | otherwise = 27 + ord c - ord 'A'

main :: IO ()
main = do
  contents <- readFile "day03/input.txt"
  let parsed = lines contents
      res1 = foldl' (\x y -> x + (priority . commonItem . splitRucksack) y) 0 parsed
      res2 = foldl' (\x y -> x + (priority . commonItem) y) 0 $ groupRucksacks parsed
  mapM_ print [res1, res2]
