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

