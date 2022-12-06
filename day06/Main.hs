import qualified Data.ByteString as B
import Debug.Trace

distinctSubsequence :: B.ByteString -> Int -> Bool
distinctSubsequence _ 1 = True
distinctSubsequence bs n = let h = B.head bs
                               t = B.tail bs
                               equalityTest = B.all (h /=) $ B.take (n-1) t
                           in equalityTest && distinctSubsequence t (n-1)

findStartOfSequence :: B.ByteString -> Int -> Int -> Int
findStartOfSequence bs n i = if distinctSubsequence bs n
                             then i + n
                             else findStartOfSequence (B.tail bs) n (i+1)
                                  
main :: IO ()
main = do
  contents <- B.readFile "day06/input.txt"
  let res1 = findStartOfSequence contents 4 0
      res2 = findStartOfSequence contents 14 0
  mapM_ print [res1, res2]
