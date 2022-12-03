import qualified Data.Set as S
import Data.Char
import Data.Foldable

splitRucksack :: Ord a => [a] -> (S.Set a, S.Set a)
splitRucksack r = let compartments  = splitAt (length r `div` 2) r
                  in (\(x,y) -> (S.fromList x, S.fromList y)) compartments

groupRucksacks :: Ord a => [[a]] -> [(S.Set a, S.Set a, S.Set a)]
groupRucksacks (x:y:z:rest) = (S.fromList x, S.fromList y, S.fromList z) : groupRucksacks rest
groupRucksacks _ = []

commonItem :: Ord a => (S.Set a, S.Set a) -> a
commonItem (x, y) = head $ S.elems $ S.intersection x y

commonItem3 :: Ord a => (S.Set a, S.Set a, S.Set a) -> a
commonItem3 (x, y, z) = head $ S.elems $ S.intersection (S.intersection x y) z

priority :: Char -> Int
priority c
  | c >= 'a' = 1 + ord c - ord 'a'
  | otherwise = 27 + ord c - ord 'A'

main :: IO ()
main = do
  contents <- readFile "day03/input.txt"
  let parsed = lines contents
      res1 = foldl' (\x y -> x + (priority . commonItem . splitRucksack) y) 0 parsed
      res2 = foldl' (\x y -> x + (priority . commonItem3) y) 0 $ groupRucksacks parsed
  mapM_ print [res1, res2]
