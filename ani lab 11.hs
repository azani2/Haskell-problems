
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}

import Prelude hiding (lookup)

g :: (Eq a, Num a) => [(a, [a])]
g =
  [ (1, [2, 3]),
    (2, [4, 1]),
    (3, [4]),
    (4, [5]),
    (5, [4])
  ]

data Nat -- от Natural number (естествено число)
  = Zero
  | Succ Nat
  deriving (Show)

-- Двоично дърво:
data BTree a
  = Leaf
  | Node a (BTree a) (BTree a)
  deriving (Show)

predNat :: Nat -> Nat
predNat Zero = Zero
predNat (Succ n) = n

integerToNat :: Integer -> Nat
integerToNat 0 = Zero
integerToNat n = Succ (integerToNat (n - 1))

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

plus :: Nat -> Nat -> Nat
plus Zero m = m
plus (Succ n) m = Succ (plus n m)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ n) m = plus m (mult n m)

safeDiv :: Int -> Int -> Maybe (Int, Int)
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `quot` y, x `rem` y)

isSquareMatrix :: [[a]] -> Bool
isSquareMatrix xss = all ((== length xss) . length) xss

mainDiag :: [[a]] -> [a]
mainDiag [] = []
mainDiag ([] : _) = []
mainDiag ((y:_) : xs) = y : mainDiag (map tail xs)

secondaryDiag :: [[a]] -> [a]
secondaryDiag = mainDiag . reverse

lookup :: Eq k => k -> [(k, v)] -> Maybe v
lookup _ [] = Nothing
lookup k (kv : kvs)
    | fst kv == k = Just $ snd kv
    | otherwise = lookup k kvs

outDeg :: Eq a => [(a, [a])] -> a -> Int
outDeg nodes x = maybe 0 length $ lookup x nodes

inDeg :: Eq a => a -> [(a, [a])] -> Int
inDeg x = foldl op 0
    where
        op acc (_, children)
            | x `elem` children = acc + 1
            | otherwise = acc

edge :: Eq a => [(a, [a])] -> a -> a -> Bool
edge nodes a b = any isEdge nodes
    where 
        isEdge (x, children) = x == a && b `elem` children

path :: Eq a => [(a, [a])] -> a -> a -> Bool
path graph a b
    | a == b = True
    | otherwise = 
        let succs = lookup a graph
            in case succs of
                Nothing -> False
                Just children -> any (\c -> path graph c b) children

main :: IO ()
main = print $ lookup 5 [(10, 'a'), (5,'c')]

