import Part2
import System.Random
import Control.Monad(replicateM)

-- Problem 21: Insert an element at a given position into a list
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs i = take (pred i) xs ++ (x:drop (pred i) xs)

-- Problem 22: Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range start end
  | start < end = start:range (succ start) end
  | start == end = [end]
  | start > end = range end start

-- Problem 23: Extract a given number of randomly selected elements from a list.
-- Currently returns a list of random indices in the list
randomElements :: [a] -> Int -> IO [a]
randomElements x n = do
            ri <- replicateM n $ getStdRandom $ randomR (0, length x - 1)
            return (map (x !!) ri)
