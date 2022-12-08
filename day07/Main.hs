import qualified Data.Map as M
import Data.List
import Data.List.Split

type FSElement = Either String Int
type Directory = [FSElement]
type FS = M.Map String Directory
type FSUsage = M.Map String Int

parse :: String -> FS
parse contents = let (_:rawDirs) = splitOn "$ cd " contents
                     (names, dirs) = unzip $ map (parseDir . delete "$ ls" . lines) rawDirs
                     paths = reverse $ foldl (\paths@(x:xs) y -> newFileName x y : paths) ["/"] (tail names)
                 in M.fromList . filter (\(_, x) -> x /= []) $ zip paths dirs

parseDir :: [String] -> (String, Directory)
parseDir (name:content) = (name, map parseLine content)
parseDir _ = error "Empty list"

parseLine :: String -> FSElement
parseLine line
  | x == "dir" = Left name
  | otherwise = Right (read x :: Int)
  where [x, name] = words line

newFileName :: String -> String -> String
newFileName current ".." = init $ dropWhileEnd ('/' /=) current
newFileName "/" relative = "/" ++ relative
newFileName current relative = current ++ "/" ++ relative

usage :: FS -> String -> Int
usage fs path = let dir = M.findWithDefault [] path fs
                in usage' fs path dir

usage' :: FS -> String -> Directory -> Int
usage' _ _ [] = 0
usage' fs base ((Left x):xs) = usage fs (newFileName base x) + usage' fs base xs
usage' fs base ((Right x):xs) = x + usage' fs base xs

main :: IO ()
main = do
  contents <- readFile  "day07/input.txt"
  let fs = parse contents
      fsUsage = map (usage fs) $ M.keys fs
      rootSize = maximum fsUsage
      res1 = sum . filter (100000 >) $ fsUsage
      res2 = minimum . filter (\x -> rootSize - x < 40000000) $ fsUsage
  mapM_ print [res1, res2]
