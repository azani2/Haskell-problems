{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
{-# OPTIONS_GHC -fwarn-unused-matches #-}
import Prelude

data Tree
    = Empty
    | Node Int [Tree]
    deriving (Show)

divisors :: Int -> [Int]
divisors x = [ n | n <- [2..x], x `rem` n == 0]

intersect :: Eq a => [a] -> [a] -> [a]
intersect _ [] = []
intersect [] _ = []
intersect xs ys = [n | n <- xs, n `elem` ys]

coPrime :: Int -> Int -> Bool
coPrime x y = null (xDivs `intersect` yDivs)
    where    
        xDivs = divisors x
        yDivs = divisors y

prune :: Tree -> Tree
prune Empty = Empty --fix
prune (Node r subtrees) = Node r (map prune (filter (help r) subtrees))
    where 
        help _ Empty =  False
        help node (Node x _) = coPrime x node

tree :: Tree
tree = 
    Node 
        10
        [ Node 3 [Empty, Node 3 [], Node 2 [Empty, Empty]],
          Node 7 [Node 14 [], Node 2 [], Node 1 [Empty], Node 15 [Empty]],
          Node 20 [Node 9 [Node 10 []]]
        ]

mergeFromMaybes :: [Maybe a] -> [Maybe a] -> [a]
mergeFromMaybes [] [] = []
mergeFromMaybes xs [] = mergeFromMaybes xs xs
mergeFromMaybes [] ys = mergeFromMaybes ys ys
mergeFromMaybes ((Just x):xs) (_:ys) = x : mergeFromMaybes xs ys
mergeFromMaybes (Nothing:xs) ((Just y):ys) = y : mergeFromMaybes xs ys
mergeFromMaybes (Nothing:xs) (Nothing:ys) = mergeFromMaybes xs ys

maybes1 :: [Maybe Int]
maybes1 = [Just 1, Nothing, Just 3, Nothing, Just 5, Nothing]

maybes2 :: [Maybe Int]
maybes2 = [Just 2, Nothing, Nothing, Just 8, Nothing, Nothing]

maybesInf :: [Maybe Int]
maybesInf = Just 1 : maybesInf

main :: IO ()
main = do
    print $ prune tree
    print $ divisors 7
    print $ divisors 14
    print $ intersect (divisors 7) (divisors 14)
    print $ coPrime 7 14
    print $ mergeFromMaybes maybes1 maybes2
    print $ take 10 (mergeFromMaybes maybes1 maybesInf) --works for streams

--runhaskell "malko kontrolno 2.hs"