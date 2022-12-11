import qualified Data.Sequence as S
import Data.Array
import Data.List (sort)
import Data.List.Split
import Data.Foldable

data Monkey = Monkey { items :: S.Seq [Int]
                     , inspections :: Int
                     , operation :: ([Int] -> [Int])
                     , test :: ([Int] -> Int -> Int)
                     , test' :: ([Int] -> Int -> Int)
                     , moduleN :: Int
                     }

instance Show Monkey where
  show (Monkey items inspections _ _ _ _) =
    show items ++ " " ++ show inspections

type MonkeyArray = Array Int Monkey
type MonkeyPlayFn = (Int -> MonkeyArray -> MonkeyArray)

parse :: String -> MonkeyArray
parse content = listArray (0, l - 1) monkeyList
  where rawMonkeys = splitOn "\n\n" content
        l = length rawMonkeys
        monkeyList = map (\x -> parseMonkey x l) rawMonkeys

parseMonkey :: String -> Int -> Monkey
parseMonkey rawData n =
  let [rawItems, rawOp, rawTest, rawThen, rawElse] = tail . lines $ rawData
      itemSeq = S.fromList . map (replicate n . read) .
        splitOn "," . tail . dropWhile (':' /=) $ rawItems :: S.Seq [Int]
      [op1, f, op2] = words . tail . dropWhile ('=' /=) $ rawOp
      operationFn = buildOperation (op1, f, op2)
      [m, a, b] = map (read . last . words) [rawTest, rawThen, rawElse] :: [Int]
      testFn = buildTest m a b
      testFn' = buildTest' a b
  in Monkey itemSeq 0 operationFn testFn testFn' m

buildTest :: Int -> Int -> Int -> [Int] -> Int -> Int
buildTest m a b n i = if (head n) `mod` m == 0 then a else b

buildTest' :: Int -> Int -> [Int] -> Int -> Int
buildTest' a b n i = if (n !! i) == 0 then a else b

buildOperation :: (String, String, String) -> [Int] -> [Int]
buildOperation ("old", "*", "old") n = map (\x -> x * x) n
buildOperation ("old", "*", x) n = map ((read x :: Int) *) n
buildOperation (_, _, x) n = map ((read x :: Int) +) n

monkeyPop :: Monkey -> ([Int], Monkey)
monkeyPop (Monkey (n S.:<| rest) a b c d e) = (n, Monkey rest (a+1) b c d e)
monkeyPop _ = error "Empty queue"

monkeyEnque :: [Int] -> Monkey -> Monkey
monkeyEnque n (Monkey queue a b c d e) = Monkey (queue S.|> n) a b c d e

monkeyPlay :: Int -> MonkeyArray -> MonkeyArray
monkeyPlay i arr = let (n, m) = monkeyPop (arr ! i)
                       results = operation m n
                       n' = map (\x -> x `div` 3) results
                       dest = test m n' i
                       m' = monkeyEnque n' (arr ! dest)
                   in arr // [(i, m), (dest, m')]

monkeyPlay' :: Int -> MonkeyArray -> MonkeyArray
monkeyPlay' i arr = let (n, m) = monkeyPop (arr ! i)
                        modules = toList . fmap (moduleN) $ arr
                        results = operation m n
                        n' = zipWith (\x y -> x `mod` y) results modules
                        dest = test' m n' i
                        m' = monkeyEnque n' (arr ! dest)
                    in arr // [(i, m), (dest, m')]

monkeysPlayRounds :: MonkeyArray -> Int -> MonkeyPlayFn -> MonkeyArray
monkeysPlayRounds arr n play =
  iterate (\x -> foldl
            (\a i ->
                if items (a ! i) == S.empty
                then a
                else let l = S.length $ items (a ! i)
                     in iterate (play i) a !! l
            ) x [0..b]) arr !! n
  where (_, b) = bounds arr

monkeyBusiness :: MonkeyArray -> Int
monkeyBusiness = product . take 2 . reverse . sort . toList . fmap (inspections)

main :: IO ()
main = do
  contents <- readFile "day11/input.txt"
  let monkeyArr = parse contents
      res1 = monkeyBusiness $ monkeysPlayRounds monkeyArr 20 monkeyPlay
      res2 = monkeyBusiness $ monkeysPlayRounds monkeyArr 10000 monkeyPlay'
  mapM_ print [res1, res2]
