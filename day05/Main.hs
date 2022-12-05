import qualified Data.Text as T
import qualified Data.Map as M
import Data.Char

parseStacks :: [T.Text] -> M.Map Int T.Text
parseStacks rawStacks = let stackLength = length . parseStackLine $ head rawStacks
                            keys = [1..stackLength]
                            values = map T.concat $ joinStacks $ init rawStacks
                        in M.fromList $ zip keys values

joinStacks :: [T.Text] -> [[T.Text]]
joinStacks (x:xs) = zipWith (:) (parseStackLine x) (joinStacks xs)
joinStacks _ = repeat []

parseStackLine :: T.Text -> [T.Text]
parseStackLine line = let splitLine = T.chunksOf 4 line
                          dropAroundFn = (\x -> isSpace x || x == '[' || x == ']')
                      in map (T.dropAround dropAroundFn) splitLine

parseMoveLine :: T.Text -> (Int, Int, Int)
parseMoveLine line = let (_:x:_:y:_:z:_) = T.words line
                         [x', y', z'] = map (read . T.unpack) [x, y, z] :: [Int]
                     in (x', y', z')

makeMovementN :: M.Map Int T.Text -> (Int, Int, Int) -> M.Map Int T.Text
makeMovementN stacks (0, _, _) = stacks
makeMovementN stacks (n, from, to) = makeMovementN (makeMovement stacks (from, to)) (n-1, from, to)

makeMovement :: M.Map Int T.Text -> (Int, Int) -> M.Map Int T.Text
makeMovement stacks (from, to) = let crate = T.head $ M.findWithDefault (T.pack " ") from stacks
                                     removed = M.adjust T.tail from stacks
                                 in M.adjust (\a -> crate `T.cons` a) to removed

result :: M.Map Int T.Text -> String
result stacks = map T.head $ M.elems stacks

makeMovement' :: M.Map Int T.Text -> (Int, Int, Int) -> M.Map Int T.Text
makeMovement' stacks (n, from, to) = let crates = T.take n $ M.findWithDefault (T.pack " ") from stacks
                                         removed = M.adjust (T.drop n) from stacks
                                     in M.adjust (T.append crates) to removed
main :: IO ()
main = do
  contents <- readFile "day05/input.txt"
  let [rawStacks, rawMoves] = map T.lines $ T.splitOn (T.pack "\n\n") $ T.pack contents
      (stacks, moves) = (parseStacks rawStacks, map parseMoveLine rawMoves)
      res1 = result $ foldl makeMovementN stacks moves
      res2 = result $ foldl makeMovement' stacks moves
  mapM_ print [res1, res2]
