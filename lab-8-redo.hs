{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
{-# OPTIONS_GHC -fwarn-unused-matches #-}

import Prelude hiding (abs, filter, map, revrse)

abs :: Int -> Int
abs n = if n >= 0 then n else -n

apply' :: (a -> b) -> a -> b
apply' = id

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

prefix :: [Int] -> [Int] -> Bool
prefix xs ys = take (length xs) ys == xs

suffix :: [Int] -> [Int] -> Bool
suffix xs ys = drop (length ys - length xs) ys == xs

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x acc -> if p x then x : acc else acc) []

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x acc -> f x : acc) []

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

weakListComprehension :: (a -> b) -> (a -> Bool) -> [a] -> [b]
weakListComprehension f p = map f . filter p

closed :: (Int -> Int) -> [Int] -> [Int]
closed f xs = filter (\x -> f x `elem` xs) xs

uninterleave :: [a] -> ([a], [a])
uninterleave = foldr (\x (xs, ys) -> (x : ys, xs)) ([], [])

main :: IO ()
main = print (closed abs [1, -2, -5, 7, -1])