import Part1

-- Problem 11: Modified run length encoding
-- -----------
-- Desc: Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied
-- into the result list. Only elements with duplicates are transferred as (N E) lists.

--Note: Discovered I'm terribly weak at defining and using custom data types
data EncodeList a = Single a | Multiple Int a deriving (Show)
encodeModified :: Eq a => [a] -> [EncodeList a]
encodeModified x = map pairToEncodeList (encode x)
                    where pairToEncodeList (1, i) = Single i
                          pairToEncodeList (n, i) = Multiple n i


-- Problem 12: Run length decoding
-- -----------
-- Desc: Given a run-length encoding code list generated as specified in Problem 11, construct it's uncompressed version
decodeModified :: [EncodeList a] -> [a]
decodeModified = concatMap decodeMod'
                 where
                   decodeMod' (Single i) = [i]
                   decodeMod' (Multiple n i) = replicate n i


-- Problem 13: Run-length encoding (direct)
-- Example:
--  > encodeDirect "aaabbc"
-- => [Multiple 3 'a', Multiple 2 'b', Single 'c']
encodeDirect' :: Eq a => [a] -> [[a]]
encodeDirect' [] = []
encodeDirect' x = takeWhile (==(head x)) x : encodeDirect' (dropWhile (==(head x)) x)

toEncodeList  :: [y] -> (EncodeList y)
toEncodeList y
  | (length y) == 1  = Single (head y)
  | otherwise = Multiple (length y) (head y)

encodeDirect :: Eq a => [a] -> [EncodeList a]
encodeDirect x = map toEncodeList (encodeDirect' x)


-- Problem 14: Duplicate a list
duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = x:x:(duplicate xs)

-- Problem 15: Replicate each element in a list a given number of times
-- Prelude imports another replicate function so naming this repli

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n
  | n < 0 = error "Array cannot be replicated negative times"
  | otherwise = (repli' x n) ++ (repli xs n)
                 where repli' x n
                         | n <= 0 = []
                         | otherwise = x : (repli' x (pred n))

-- Problem 16: Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery x n
  | n <= 0 = x
  | length x >= n = init (take n x) ++ (dropEvery (drop n x) n)
  | otherwise = x

-- Problem 17: Split a list into two parts; the length of the first part is given.
split :: [a] -> Int -> ([a], [a])
split x n = ((take n x), (drop n x))

-- Problem 18:  Extract a slice from a list.
-- Note: Both indices inclusive, indexing starts from 1.
slice :: [a] -> Int -> Int -> [a]
slice a i k
  | k < i = slice a k i
  | otherwise = take (k - (pred i)) (drop (pred i) a)

-- Problem 19:  Rotate a list N places to the left.
rotate :: [a] -> Int -> [a]
rotate x n
  | n > 0 = drop n x ++ take n x
  | n < 0 = rotate x (length x + n)
  | otherwise = x

-- Problem 20: Remove the K'th element from a list.
removeAt :: Int -> [a] -> (a, [a])
removeAt n x = ( x !! (pred n), take (pred n) x ++ (drop n x))
