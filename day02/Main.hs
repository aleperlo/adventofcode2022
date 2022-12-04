import qualified Data.Map as M
import Data.Char

parse :: String -> [(Char, Char)]
parse content = map (\(x:_:y:[]) -> (x, chr $ ord y - ord 'X' + ord 'A')) $ lines content

valueMap :: M.Map (Char, Char) Int
valueMap = M.fromList[(('A','A'), 3+1), (('A','B'), 6+2), (('A','C'), 0+3),
                      (('B','A'), 0+1), (('B','B'), 3+2), (('B','C'), 6+3),
                      (('C','A'), 6+1), (('C','B'), 0+2), (('C','C'), 3+3)
                     ]

getValue :: (Char, Char) -> Int
getValue pair = M.findWithDefault 0 pair valueMap

score :: [(Char, Char)] -> Int
score pairs = foldl (\x y -> (+) x $ getValue y) 0 pairs

fixPair :: (Char, Char) -> (Char, Char)
fixPair (x, 'A') = (x, chr $ (ord x - ord 'A' + 2) `mod` 3 + ord 'A')
fixPair (x, 'B') = (x, x)
fixPair (x, 'C') = (x, chr $ (ord x - ord 'A' + 1) `mod` 3 + ord 'A')
fixPair _ = error "The last member of the pair is not valid"

main :: IO ()
main = do
  contents <- readFile "day02/input.txt"
  let parsed = parse contents
      res1 = score parsed
      res2 = score $ map fixPair parsed
  mapM_ print [res1, res2]
