import Data.List
-- problem 1

myLast :: [a] -> a
myLast [] = error "No end for empty lists!"
myLast [x] = x
myLast (_:xs) = myLast xs

--problem 2

myButLast :: [a] -> a
myButLast [] = error "Empty list!"
myButLast [x] = error "Too few elements!"
myButLast x = reverse x !! 1

--problem 3

elementAt :: [a] -> Int -> a
elementAt [] _ = error "Empty list"
elementAt (x:_) 1 = x
elementAt (_:xs) i = elementAt xs (i-1)

--problem 4

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = myLength xs + 1

--problem 5

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

--problem 6

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = if xs == (myReverse xs) then True
					else False

--problem 7

data NestedList a = Elem a | List [NestedList a]
myflatten :: NestedList a -> [a]
myflatten (List []) = []
myflatten (Elem  a) = [a]
myflatten (List (x:xs)) = myflatten x ++ myflatten (List xs)

--problem 8

compress :: Eq a => [a] -> [a]
compress = map head . group

--problem 9

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [a] = [[a]]
pack (x:xs) = if x `elem` (head (pack xs))
				then (x:(head (pack xs))):(tail (pack xs))
				else [x]:(pack xs)

--problem 10

encode xs = map (\x -> (length x,head x)) (group xs)

--problem 11
data ListItem a = Single a | Multiple Int a
    deriving (Show)
encodeModified xs = [y | x <- group xs, let y = if (length x) == 1 then Single (head x) else Multiple (length x) (head x)]