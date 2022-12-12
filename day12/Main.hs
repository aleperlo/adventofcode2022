import qualified Data.Set as Set
import Data.Array

type Pos = (Int, Int)
type HeightMap = Array Pos Char
type PosSet = Set.Set Pos
type ConstraintFn = Char -> Char -> Bool

parse :: String -> HeightMap
parse content = let splitContent = lines content
                    bound = (length splitContent, length . head $ splitContent)
                in listArray ((1, 1), bound) $ concat splitContent

findPos :: HeightMap -> Char -> Pos
findPos hm c = fst . head . filter (\(pos, c') -> c == c') $ assocs hm

findPath :: HeightMap -> PosSet -> Int -> Pos -> PosSet -> Int
findPath hm vs d dest ex
  | dest `elem` ex = d
  | otherwise = let vs' = Set.union vs ex
                    ex' = Set.unions . map (legalMoves hm constraint vs') . Set.elems $ ex
                in findPath hm vs' (d+1) dest ex'

findPath' :: HeightMap -> PosSet -> Int -> PosSet -> Int
findPath' hm vs d ex
  | any (\x -> 'a' == hm ! x) ex = d
  | otherwise = let vs' = Set.union vs ex
                    ex' = Set.unions . map (legalMoves hm constraint' vs') . Set.elems $ ex
                in findPath' hm vs' (d+1) ex'

constraint :: Char -> Char -> Bool
constraint c c' = c' <= succ c

constraint' :: Char -> Char -> Bool
constraint' c c' = c' >= pred c

legalMoves :: HeightMap -> ConstraintFn -> PosSet -> Pos -> PosSet
legalMoves hm constraint vs p@(r,c) =
  let current = hm ! p
      ((minr, minc), (maxr, maxc)) = bounds hm
      steps = [(r+1,c), (r,c+1), (r-1,c), (r,c-1)]
  in Set.fromList $ filter
     (\p'@(x, y) ->
         if (x >= minr && x <= maxr) && (y >= minc && y <= maxc)
         then constraint current (hm ! p') && p' `Set.member` vs == False
         else False
     ) steps

main :: IO ()
main = do
  contents <- readFile "day12/input.txt"
  let heightMap = parse contents
      [start, end] = map (findPos heightMap) ['S', 'E']
      heightMap' = fmap (\x -> case x of
                                 'S' -> 'a'
                                 'E' -> 'z'
                                 otherwise -> x
                        ) heightMap
      res1 = findPath heightMap' Set.empty 0 end (Set.singleton start)
      res2 = findPath' heightMap' Set.empty 0 (Set.singleton end)
  mapM_ print [res1, res2]
