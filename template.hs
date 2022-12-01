main :: IO ()
main = do
  contents <- readFile "dayXX/input.txt"
  let parsed = parse contents
      res1 = 0
      res2 = 0
  mapM_ print [res1, res2]
