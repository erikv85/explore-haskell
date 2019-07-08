import System.Environment (getArgs)

import qualified FinLib as Fin

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

gloo :: ([(Double, Double)] -> t) -> [Security] -> [(String, t)]
gloo _ [] = []
gloo f (sec : secs) = let n = name sec
                          ps = purchases sec
                          dbls = map toNum ps
                       in (n, f dbls) : (gloo f secs)

lol :: Show a => [a] -> String
lol l = '[' : lol' l ++ "]"
          where lol' [] = ""
                lol' [x] = show x
                lol' (x0:x1:xs) = show x0 ++ "\n" ++ lol' (x1:xs)

main = do
    args <- getArgs
    let file = case args of
                 [] -> "dummy-data.txt"
                 (hd:_) -> hd

    input <- readFile file
    let fmtd :: [Security]
        fmtd = read input
        b = map purchases fmtd
        a = (map . map) toNum b
        x = map Fin.principal a
        y = map Fin.foobaz a
        z = map(\(n, d) -> 1 + n / d) (zip (tail (y !! 0)) (init (x !! 0)))
    putStrLn $ lol (gloo Fin.foobaz fmtd)
