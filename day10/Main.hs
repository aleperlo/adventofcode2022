import Data.List.Split

parseLine :: String -> (Int, Int)
parseLine line
  | line == "noop" = (0, 0)
  | otherwise = let (_, n) = splitAt 5 line
                in (1, read n :: Int)

lookupRegister :: [Int] -> (Int, Int) -> [Int]
lookupRegister values@(v:_) (t, n)
  | t == 0 = v:values
  | otherwise = (v+n):v:values
lookupRegister [] _ = error "Empty list"

signalStrength :: [(Int, Int)] -> [Int] -> Int
signalStrength values indexes = foldl
  (\x (iy, y) -> if iy `elem` indexes
                 then x + (y*iy) else x)
  0 values

draw :: (Int, Int) -> Char
draw (a, b) = if abs (mod (a-1) 40 - b) <= 1 then '#' else '.'

main :: IO ()
main = do
  contents <- readFile "day10/input.txt"
  let instr = map parseLine $ lines contents
      registerValues = zip [1,2..] (reverse $ foldl lookupRegister [1] instr)
      res1 = signalStrength registerValues [20,60..220]
      res2 = unlines . chunksOf 40 $ (init . map draw) registerValues
  print res1
  putStr res2
