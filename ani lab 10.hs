import Prelude hiding (cycle, iterate, repeat, scan1)

nats :: [Integer]
nats = 0 : map succ nats

repeat :: a -> [a]
repeat x = x : repeat x

iterate :: (a -> a) -> a -> [a]
iterate f x = f x : iterate f (f x)

cycle :: [a] -> [a]
cycle xs = xs ++ cycle xs

scan1 :: (b -> a -> b) -> b -> [a] -> [b]
scan1 _ acc [] = []
scan1 op acc (x:xs) = op acc x : scan1 op (op acc x) xs

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

facts :: [Integer]
facts = scan1 (*) 1 (drop 1 nats)

primes :: [Integer]
primes = next [2..]
    where
        next (x:xs) = x : next (filter ((/=0) . (`rem` x)) xs)

pythagoreanTriples :: [(Int, Int, Int)]
pythagoreanTriples =
    [(a, b, c) | a <- [1 ..], b <- [1 .. a], c <- [1 .. a], p a b c]
        where
            p x y z = x ^ 2 == y ^ 2 + z ^ 2

natTuples :: [(Int, Int)]
natTuples = concatMap diag [0 ..]
    where
        diag n = [(x, y) | x <- [0 .. n], y <- [0 .. n], x + y == n]

compositions :: String -> String -> [String]
compositions f g = next ["id"]
  where
    next xs = xs ++ next (concatMap (\x -> [x ++ " . " ++ f, x ++ " . " ++ g]) xs)
    

filterTuples :: (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
filterTuples p xs ys = filter (uncurry p) cartesian
    where
        cartesian = map (\(x, y) -> (xs !! x, ys !! y)) natTuples



main :: IO ()
main = print (take 10 pythagoreanTriples)