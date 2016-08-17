import Part2
import System.Random

-- Problem 21: Insert an element at a given position into a list
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs i = take (pred i) xs ++ (x:(drop (pred i) xs))

-- Problem 22: Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range start end
  | start < end = start:(range (succ start) end)
  | start == end = [end]
  | start > end = range end start

-- Problem 23: Extract a given number of randomly selected elements from a list.
-- needs understanding of monads and IO
