
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
-- 	      The first element in the list is number 1.
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
	