import qualified Data.Text as T
import qualified Data.Map as M
import Data.Char

type Movement = (Int, Int, Int)
type StackMap = M.Map Int T.Text

parseStacks :: [T.Text] -> StackMap
parseStacks rawStacks =
  let values = map T.concat . joinStacks $ init rawStacks
  in M.fromList $ zip [1..length values] values

joinStacks :: [T.Text] -> [[T.Text]]
joinStacks (x:xs) = zipWith (:) (parseStackLine x) (joinStacks xs)
joinStacks _ = repeat []

parseStackLine :: T.Text -> [T.Text]
parseStackLine line =
  let splitLine = T.chunksOf 4 line
      dropAroundFn = (\x -> isSpace x || x == '[' || x == ']')
  in map (T.dropAround dropAroundFn) splitLine

parseMoveLine :: T.Text -> Movement
parseMoveLine line =
  let (_:x:_:y:_:z:_) = T.words line
      [x', y', z'] = map (read . T.unpack) [x, y, z] :: [Int]
  in (x', y', z')

makeMovement :: StackMap -> Movement -> StackMap
makeMovement stacks (0, _, _) = stacks
makeMovement stacks (n, from, to) =
  let updated = makeMovement' stacks (1, from, to)
  in makeMovement updated (n-1, from, to)

makeMovement' :: StackMap -> Movement -> StackMap
makeMovement' stacks (n, from, to) =
  let crates = T.take n $ M.findWithDefault (T.pack "") from stacks
      removed = M.adjust (T.drop n) from stacks
  in M.adjust (T.append crates) to removed

result :: StackMap -> String
result stacks = map T.head $ M.elems stacks

main :: IO ()
main = do
  contents <- readFile "day05/input.txt"
  let [rawStacks, rawMoves] = map T.lines $ T.splitOn (T.pack "\n\n") $ T.pack contents
      (stacks, moves) = (parseStacks rawStacks, map parseMoveLine rawMoves)
      res1 = result $ foldl makeMovement stacks moves
      res2 = result $ foldl makeMovement' stacks moves
  mapM_ print [res1, res2]
