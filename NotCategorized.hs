module NotCategorized where

import Text.Printf (printf)

type NamedSeries = (String, [Double]) -- for later use ?

data Security = Security String [Purchase] deriving (Show, Read)

name :: Security -> String
name (Security n _) = n

purchases :: Security -> [Purchase]
purchases (Security _ l) = l

-- use this style rather than records to minimize effort
-- involved in writing data files by hand
data Purchase = Purchase String Double Double deriving (Show, Read)

date :: Purchase -> String
date (Purchase d _ _) = d

price :: Purchase -> Double
price (Purchase _ p _) = p

pieces :: Purchase -> Double
pieces (Purchase _ _ x) = x

-- temporary dummy method since FinLib doesn't use `Purchase`
toNum :: Purchase -> (Double, Double)
toNum p = (price p, pieces p)

toNum' :: Purchase -> (String, Double, Double)
toNum' p = (date p, price p, pieces p)

toPurchase :: (String, Double, Double) -> Purchase
toPurchase (d,pr,pi) = Purchase d pr pi

-- use with sort . nub . concat
allDates :: [Security] -> [[String]]
allDates s = map (\sec -> map date (purchases sec)) s

gloo :: ([(Double, Double)] -> t) -> [Security] -> [(String, t)]
gloo _ [] = []
gloo f (sec : secs) = let n = name sec
                          ps = purchases sec
                          dbls = map toNum ps
                       in (n, f dbls) : (gloo f secs)

-- Takes two lists l and ll=zip3 a _ _ such that a is subset
-- of l and a !! 0 == l !! 0
--interpolate' :: Eq a => [a] -> [(a,b,c)] -> (b, c) -> [(a,b,c)]
interpolate' [] _ _ = []
interpolate' (x:xs) [] mr@(m,n) = (x, m, n) : (interpolate' xs [] mr)
interpolate' (x:xs) ll@(y@(a,b,c):ys) mr@(m,n) = if x == a
                                                    then y : (interpolate' xs ys (b, 0))
                                                    else (x, m, 0) : (interpolate' xs ll mr)

interpolateAllSecs :: [Security] -> [String] -> [Security]
interpolateAllSecs [] _ = []
interpolateAllSecs (sec:secs) ds = let n = name sec
                                       ps = purchases sec
                                       dbls = map toNum' ps
                                       ild = interpolate' ds dbls (0.0, 0.0)
                                       nps = map toPurchase ild
                                       nsec = Security n nps
                                    in nsec : interpolateAllSecs secs ds

weights :: [(String, [Double])] -> [Double] -> [(String, [Double])]
weights [] _ = []
weights ((n,v):vs) pf = let ws = map (\(x,y) -> x/y) (zip v pf)
                         in (n, ws) : weights vs pf

percent :: [(String, [Double])] -> [(String, [Double])] -> [(String, [Double])]
percent [] _ = []
percent ((n,v):vs) ((n',v'):vs') = let pc = map (\(x,y) -> x/y) (zip (tail v) (init v'))
                                    in (n,pc) : percent vs vs'

lol :: Show a => [a] -> String
lol l = '[' : lol' l ++ "]"
          where lol' [] = ""
                lol' [x] = show x
                lol' (x0:x1:xs) = show x0 ++ "\n" ++ lol' (x1:xs)

lol2 :: Show a => [(a, [Double])] -> String
lol2 l = '[' : lol2' l ++ "]"
           where lol2' [] = ""
                 lol2' [(x,y)] = show x ++ "," ++ lol3 y
                 lol2' (x0:x1:xs) = lol2' [x0] ++ "\n" ++ lol2' (x1:xs)

-- TODO: add format parameter
lol3 :: [Double] -> String
lol3 l = '[' : lol3' l ++ "]"
           where lol3' [] = ""
                 lol3' [x] = printf "%.2f" x
                 lol3' (x0:x1:xs) = lol3' [x0] ++ "," ++ lol3' (x1:xs)

f :: [[Double]] -> [Double]
f [] = []
f l@(x:xs)
  | x == []   = []
  | otherwise = sum (map head l) : (f (map tail l))
