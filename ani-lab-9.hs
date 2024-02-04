-- cover all cases!
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}

import Prelude hiding (foldl, map, pi, takeWhile, zip, zipWith)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc [] = acc
foldl op acc (x:xs) = foldl op (op acc x) xs

zip :: [a] -> [b] ->[(a, b)]
zip _ [] = []
zip [] _ = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) = if p x then x : takeWhile p xs else []

nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/=x) xs)

prime :: Int -> Bool
prime n
    | n < 2 = False
    | otherwise = null [x | x<-[2..n-1], n `rem` x == 0]

factorize :: Int -> [Int]
factorize 1 = []
factorize n =
    let primeDivs = [x | x<-[2..n], prime x, n `rem` x == 0]
        firstDiv = head primeDivs
    in firstDiv : factorize (n `div` firstDiv)

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = smaller ++ x : bigger
    where smaller = quicksort [y | y<-xs, y <=  x]
          bigger = quicksort [y | y<-xs, y > x]


main :: IO ()
main = print "done"