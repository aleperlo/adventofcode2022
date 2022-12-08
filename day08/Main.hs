import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.List.Split

type Pos = (Int, Int)
type Forest = M.Matrix Int
type TreeSet = S.Set Pos
type TreeLine = V.Vector Int

data Side = T | B | L | R deriving (Eq)

parse :: String -> (Forest, Pos)
parse contents =
  let splitContents = map (map read . chunksOf 1) $ lines contents
      nr = length $ splitContents
      nc = length . head $ splitContents
  in (M.fromLists splitContents, (nr, nc))

checkSide :: Side -> Int -> Pos -> Forest -> TreeSet -> TreeSet
checkSide s p bounds@(nr, nc) fs ts
  | (p == nr+1 && (s == T || s == B)) || (p == nc+1 && (s == L || s == R)) = ts
  | otherwise =
      let vZip = \p (x,_) -> (x,p)
          hZip = \p (y,_) -> (p,y)
          (treeLine, posShift, zipFn) = case s of
            T -> (M.getCol p fs, (1 +), vZip)
            B -> (V.reverse $ M.getCol p fs, (nr -), vZip)
            L -> (M.getRow p fs, (1 +), hZip)
            R -> (V.reverse $ M.getRow p fs, (nc -), hZip)
          newTs = ts `S.union` visible treeLine p posShift zipFn
      in checkSide s (p+1) bounds fs newTs

visible :: TreeLine -> Int -> (Int -> Int) -> (Int -> Pos -> Pos) -> TreeSet
visible line p posShift zipFn =
  let indexedLine = V.map (\(i,v) -> (posShift i, v)) $ V.indexed line
      (first, rest) = (V.head indexedLine, V.tail indexedLine)
      visibleL = V.foldl (\trees@((_, v0):_) (i, v) -> if v > v0
                           then (i, v):trees
                           else trees
                         ) [first] rest
  in S.fromList $ map (zipFn p) visibleL

enumVisible :: Forest -> Pos -> TreeSet
enumVisible fs bounds = S.unions $ map (\x -> checkSide x 1 bounds fs S.empty) [T, B, L, R]

scenicScore :: Forest -> Pos -> Pos -> Int
scenicScore fs (nr, nc) (x, y) =
  let height = fs M.! (x, y)
      (row1, row2) = V.splitAt y $ M.getRow x fs
      (col1, col2) = V.splitAt x $ M.getCol y fs
      [row1', col1'] = map (V.reverse . V.init) [row1, col1]
      lengths = map (lowerLen height) [row1', row2, col1', col2]
  in product lengths

lowerLen :: Int -> TreeLine -> Int
lowerLen h v = let lTot = V.length v
                   l = V.length $ V.takeWhile (h >) v
                   inc = if l < lTot then 1 else 0
               in l + inc

main :: IO ()
main = do
  contents <- readFile "day08/input.txt"
  let (forest, bounds) = parse contents
      visibleTrees = S.toList $ enumVisible forest bounds
      res1 = length visibleTrees
      res2 = maximum $ map (scenicScore forest bounds) visibleTrees
  mapM_ print [res1, res2]
