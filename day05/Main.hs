import qualified Data.Text as T
import qualified Data.Sequence as S
import Data.Foldable
import Data.Char

type Movement = (Int, Int, Int)
type StackSeq = S.Seq T.Text

parseStacks :: [T.Text] -> StackSeq
parseStacks rawStacks =
  let values = map T.concat . joinStacks $ init rawStacks
  in S.fromList values

joinStacks :: [T.Text] -> [[T.Text]]
joinStacks (x:xs) =
  let splitLine = T.chunksOf 4 x
      dropAroundFn = (\x -> isSpace x || x == '[' || x == ']')
      x' = map (T.dropAround dropAroundFn) splitLine
  in zipWith (:) x' (joinStacks xs)
joinStacks _ = repeat []

parseMoveLine :: T.Text -> Movement
parseMoveLine line =
  let (_:x:_:y:_:z:_) = T.words line
      [x', y', z'] = map (read . T.unpack) [x, y, z] :: [Int]
  in (x', y', z')

makeMovement :: StackSeq -> Movement -> StackSeq
makeMovement stacks (0, _, _) = stacks
makeMovement stacks (n, from, to) =
  let updated = makeMovement' stacks (1, from, to)
  in makeMovement updated (n-1, from, to)

makeMovement' :: StackSeq -> Movement -> StackSeq
makeMovement' stacks (n, from, to) =
  let crates = T.take n $ S.index stacks (from-1)
      removed = S.adjust (T.drop n) (from-1) stacks
  in S.adjust (T.append crates) (to-1) removed

result :: StackSeq -> [Movement] -> (StackSeq -> Movement -> StackSeq) -> String
result stacks moves movementFn = map T.head $ toList finalConf
  where finalConf = foldl movementFn stacks moves

main :: IO ()
main = do
  contents <- readFile "day05/input.txt"
  let [rawStacks, rawMoves] = map T.lines $ T.splitOn (T.pack "\n\n") $ T.pack contents
      (stacks, moves) = (parseStacks rawStacks, map parseMoveLine rawMoves)
      res1 = result stacks moves makeMovement
      res2 = result stacks moves makeMovement'
  mapM_ print [res1, res2]
