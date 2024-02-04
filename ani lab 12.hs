{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}
import Prelude

data BTree a
    = Empty
    | Node a (BTree a) (BTree a)
    deriving (Show)

facts :: [Int]
facts = 1 : zipWith (*) facts [1..]

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

insertOrdered :: Ord a => a -> BTree a -> BTree a
insertOrdered x Empty = Node x Empty Empty
insertOrdered x (Node r lt rt) = if x < r then Node r (insertOrdered x lt) rt else Node r lt (insertOrdered x rt)

listToTree :: Ord a => [a] -> BTree a
listToTree [] = Empty
listToTree xs = foldr insertOrdered Empty xs

sumTree :: Num a => BTree a -> a
sumTree Empty = 0
sumTree (Node r lt rt) = sumTree lt + sumTree rt + r

allTree :: (a -> Bool) -> BTree a -> Bool
allTree _ Empty = True
allTree p (Node r lt rt) = p r && allTree p lt && allTree p rt

treeToList :: BTree a -> [a]
treeToList Empty = []
treeToList (Node r lt rt) = treeToList lt ++ r : treeToList rt

elemTree :: Eq a => a -> BTree a -> Bool
elemTree _ Empty = False
elemTree x (Node r lt rt) = r == x || elemTree x lt || elemTree x rt

findPredList :: (a -> Bool) -> [a] -> Maybe a
findPredList _ [] = Nothing
findPredList p (x:xs) = if p x then Just x else findPredList p xs

findPred :: (a -> Bool) -> BTree a -> Maybe a
findPred _ Empty = Nothing
findPred p t = findPredList p (treeToList t)

allPathsFromRoot :: BTree a -> [[a]]
allPathsFromRoot Empty = []
allPathsFromRoot (Node r Empty Empty) = [[r]]
allPathsFromRoot (Node r lt rt) = map (r:) (pathsInLeft ++ pathsInRight)
    where
        pathsInLeft = allPathsFromRoot lt
        pathsInRight = allPathsFromRoot rt
    
pathFromRoot :: Eq a => BTree a -> a -> Maybe [a]
pathFromRoot t x = findPredList (\xs -> last xs == x) (allPathsFromRoot t)

intersection :: Eq a => [a] -> [a] -> [a]
intersection _ [] = []
intersection [] _ = []
intersection (x:xs) (y:ys) = if x == y then x : intersection xs ys else []

path :: Eq a => BTree a -> a -> a -> [a]
path t x y = reverse toXCropped ++ drop i toYCropped
    where 
        toXCropped = crop toX
        toYCropped = crop toY
        crop (Just p) = drop n p
        crop Nothing = []
        i = if l > 0 then 1 else 0
        n = if l > 0 then l - 1 else l
        l = highestLevel toX toY
        highestLevel (Just xs) (Just ys) = length $ intersection xs ys
        highestLevel Nothing _ = 0
        highestLevel _ Nothing = 0
        toX = pathFromRoot t x
        toY = pathFromRoot t y


maxSumPath :: (Ord a, Num a) => BTree a -> a
maxSumPath t = maximum [sum (path t a b) | a <- treeToList t, b <- treeToList t]

t1 :: BTree Int
t1 = Node 20 (Node 15 (Node 10 Empty Empty) Empty)
             (Node 30 (Node 35 Empty Empty) (Node 40 Empty Empty))

t2 :: BTree Int
t2 = insertOrdered 25 t1

shortestIntervalSet :: (Num a, Ord a) => [(a, a)] -> [(a, a)]
shortestIntervalSet il = 
    [(a, b) | (a, b) <- il , b - a == shortest]
        where 
            shortest = minimum [ b - a | (a, b) <- il]

showLen :: (Show a) => a -> Int
showLen = length . show

depth :: BTree a -> Int
depth Empty = 0
depth t = maximum [length p | p <- allPathsFromRoot t]

level :: BTree a -> Int -> [Maybe a]
level Empty _ = []
level (Node r lt rt) n = nextLevel 1 (Node r lt rt)
    where 
        nextLevel _ Empty = [Nothing]
        nextLevel i (Node root left right) = if i == n
            then
                [Just root]
            else
                nextLevel (1 + i) left ++ nextLevel (1 + i) right

listLevel :: BTree a -> Int -> [a]
listLevel t n = filterLevel l
    where
        filterLevel [] = []
        filterLevel (Nothing:xs) = filterLevel xs
        filterLevel (Just x:xs) = x : filterLevel xs
        l = level t n

distinct :: Eq a => [a] -> [a]
distinct [] = []
distinct (x:xs) = x : distinct (filter (/=x) xs)

openList :: [[a]] -> [a]
openList [] = []
openList ([]:_) = []
openList ((y:_):xs) = y : openList xs

levelShortVer :: (Eq a) => BTree a -> Int -> [a]
levelShortVer t n = distinct $ openList listedNodes
    where
        listedNodes = map (take 1) post
        post = map (\xs -> if length xs >= n then drop (n - 1) xs else []) (allPathsFromRoot t)

treeWidth :: Show a => BTree a -> Int
treeWidth Empty = 0
treeWidth (Node r Empty Empty) = length $ show r
treeWidth t = 2 ^ (d - 1) + 2 ^ (d - 2)
    where
        d = depth t

printBT :: Show a => BTree a -> [String]
printBT Empty = [""]
printBT (Node r Empty Empty) = [show r]
printBT t = iter 1
    where
        iter levelNumber = if levelNumber > d 
            then []
            else printLevel levelNumber (level t levelNumber) : iter (levelNumber + 1)
        printLevel lN lvl = replicate (marg lN) '_' ++ concat (printNodes lN lvl) ++ replicate (marg lN) '_'
        printNodes lN = map (\node -> replicate (ind lN) '_' ++ printNode node)
        printNode Nothing = replicate maxLength '_'
        printNode (Just node) = show node
        ind lN = (2*marg lN + 1)*maxLength
        marg lN = (2*d - lN - 1)*maxLength
        d = depth t
        maxLength = maximum [nodeLen node | node <- treeToList t]
        nodeLen node = length $ show node

main :: IO ()
main = do
    print t1
    print t2
    mapM_ putStrLn (printBT t2)
    
    