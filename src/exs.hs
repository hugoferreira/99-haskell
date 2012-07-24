import Data.List
import Control.Arrow

myLast :: [a] -> a
myLast [a] = a
myLast (_ : xs) = myLast xs
myLastPF = head . reverse

myButLast :: [a] -> a
myButLast [a, b] = a
myButLast (_ : xs) = myButLast xs
myButLastPF = head . tail . reverse

elementAt :: [a] -> Int -> a
elementAt (x : _) 1 = x
elementAt (_ : xs) n = elementAt xs (n - 1)

myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = myLength xs + 1

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

data S a = List [S a] | Elem a

myFlatten :: S a -> [a]
myFlatten (Elem a)  = [a]
myFlatten (List xs) = concatMap myFlatten xs

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x : xs) = [x] ++ compress (filter (x /=) xs)

pack :: (Eq a) => [a] -> [[a]]
-- pack xs = pack' xs []
-- 	 where
--		pack' [] ys = ys
--		pack' xs ys = let (m, r) = span (== head xs) xs
--                       in pack' r (ys ++ [m])
pack = group

rle :: (Eq a) => [a] -> [(Int, a)]
rle = map (length &&& head $) . group

data MyEither b = Single b | Multiple Int b deriving (Show)

encodeModified :: (Eq a) => [a] -> [MyEither a]
encodeModified = map subst . rle where
		subst (1, x) = Single x
		subst (n, x) = Multiple n x

decode :: [MyEither a] -> [a]
decode = concatMap subst where
	subst (Single x) = [x]
	subst (Multiple n x) = replicate n x

dupli :: [a] -> [a]
dupli = concatMap (\x -> [x, x])

-- replicate :: Int -> a -> [a]
-- concatMap :: (a -> [b]) -> [a] -> [b]
-- concatMap . replicate :: Int -> [b] -> [b]
repli :: Int -> [a] -> [a]
repli = concatMap . replicate

main = print $ encodeModified [1, 2, 2, 2, 3, 4, 4, 4, 5]