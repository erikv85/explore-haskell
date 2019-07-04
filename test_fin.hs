import System.Environment (getArgs)

import qualified FinLib as Fin

-- use this style rather than records to minimize effort
-- involved in writing data files by hand
-- TODO: obviously gonna need name :: String
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

main = do
    args <- getArgs
    let dummyfile = if length args > 0
               then head args
               else "../tmp"
    -- how to create the dummy data if need be:
    --writeFile dummyfile "Purchase \"foo\" 1.1 3.14\n\nPurchase \"foo\" 1.2 3.15\n"

    input <- readFile dummyfile
    let inputlines = lines input
        fmtd :: [Purchase] -- necessary specialization for `read`
        fmtd = map read (filter (\x -> length x > 0) inputlines)
    let a = map toNum fmtd
    -- pretty much all set up to do math, finally
    return a
