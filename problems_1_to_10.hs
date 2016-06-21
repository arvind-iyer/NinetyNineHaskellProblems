
-- Problem 1: Find the last element of a list.
myLast :: [a] -> a
myLast [] = error "Empty list"
myLast (_:x) = last x

-- Problem 2: Find the last but one element of a list.
secondLast :: [a] -> a
secondLast [] = error "Empty list"
secondLast [x] =  error "List does is too short"
secondLast (_:x) = last ( init x )

-- Problem 3: Find the K'th element of a list.
--  The first element in the list is number 1.
elementAt :: Int -> [a] -> a
elementAt k _
	| k < 1  = error "Index not a positive integer"
elementAt k list = list !! (k-1)

-- Problem 4: Find the number of elements of a list.
myLength :: [a] -> Int
myLength list = countElems list 0
	where
		-- Pop and count until list is empty
		countElems [] t = t
		countElems (x:xs) t = countElems xs (t+1)

-- Problem 5: Reverse a list

-- I'll avoid using the reverse command, getting bored of using inbuilts
-- Feels like a cheat
myReverse :: [a] -> [a]
myReverse list = moveAndReverse list []
	where
		-- Popping first element and pushing it into the
		-- first position of the reversed list
		-- is repeated till the original is empty and reversed is filled
		moveAndReverse [] list = list
		moveAndReverse (x:xs) reversed = moveAndReverse xs (x:reversed)

-- Problem 6: Find out whether a list is a palindrome
isPalindrome :: Eq a => [a] -> Bool
-- Reusing code heh
isPalindrome list = (list  == (myReverse list))

-- Problem 7: Flatten a nested list structure

-- Solving by first defining a list that can have either elements or lists of
-- these nested lists
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concat (map flatten x)

-- Problem 8: Eliminate duplicates
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x:compress(dropWhile (==x) xs)

-- Problem 9:  Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements i
-- they should be placed in separate sublists.

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack x = let (group, rest) = span (==(head x)) x
               in group : pack rest

-- Problem 10: Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length
-- encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the
-- number of duplicates of the element E.

-- Example: encode "aaaabccaadeeee"
--          > [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
encode :: Eq a => [a] -> [(Int, a)]
encode x = map (\group -> (myLength group, head group)) (pack x)
