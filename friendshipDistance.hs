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
import Prelude

children1 :: Eq k => k -> [(k, [v])] -> [v]
children1 _ [] = []
children1 k (kv : kvs)
    | fst kv == k = snd kv
    | otherwise = children1 k kvs

distinct :: Eq a => [a] -> [a]
distinct [] = []
distinct (x:xs) = x : distinct (filter (/=x) xs)

getFriends :: String -> [(String, [String])] -> [String]
getFriends name g = distinct $ filter (/=[]) allRelationships
    where allRelationships = children1 name g ++ map (\(nm, frs) -> if name `elem` frs then nm else []) g

completeFriendships :: [(String, [String])] -> [(String, [String])]
completeFriendships g = map (\(nm, _) -> (nm, getFriends nm g)) g

countFriends :: [(String, [String])] -> [(String, Int)]
countFriends g = map (\(nm, _)-> (nm, length $ getFriends nm g)) g

--spisuk visited
--spisuk vurhove kum vsichki tehni susedi filtrirame po visited


g1 :: [(String, [String])]
g1 = [("A", ["C", "D", "E"]),
    ("B", ["E"]),
    ("C", ["A", "D", "E"]),
    ("D", ["C", "A"]),
    ("E", ["D", "B"])]

g2 :: [(String, [String])]
g2 = completeFriendships g1

main :: IO ()
main = do
    print $ completeFriendships g1
    print $ countFriends $ completeFriendships g1
    print "runhaskell friendshipDistance.hs"