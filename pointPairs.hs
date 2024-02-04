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

nats :: [Int]
nats = 0 : map succ nats

distance :: (Double, Double) -> (Double, Double) -> Double
distance (a, b) (c, d) = sqrt ((c-a)^2 + (d-b)^2)

natTuples :: [(Int, Int)]
natTuples = concatMap diag [0 ..]
    where
        diag n = [(x, y) | x <- [0 .. n], y <- [0 .. n], x + y == n]


pointPairs :: [((Int, Int), (Int, Int))]
pointPairs =
  [ ((x1, y1), (x2, y2))
    | (x1, y1) <- natTuples,
      (x2, y2) <- takeWhile (\(x, y) -> x - x1 < 3 || y - y1 < 3) natTuples,
      p x1 y1 x2 y2
  ]
  where
    p a b c d = pointSmallerThan a b c d && distanceInCorrectInterval a b c d
    pointSmallerThan a b c d = (a < c) || (a == c && b < d)

distanceInCorrectInterval :: Int -> Int -> Int -> Int -> Bool
distanceInCorrectInterval a b c d = dist > 2 && dist < 3
    where
        dist = distance (fromIntegral a, fromIntegral b) (fromIntegral c, fromIntegral d)

main :: IO ()
main = do
    print "working"
    print $ take 10 pointPairs
    print "runhaskell pointPairs.hs"