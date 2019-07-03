import FinLib

-- use this style rather than records to minimize effort
-- involved in writing data files by hand
data Purchase = Purchase String Double Double deriving (Show, Read)

date :: Purchase -> String
date (Purchase d _ _) = d

price :: Purchase -> Double
price (Purchase _ p _) = p

pieces :: Purchase -> Double
pieces (Purchase _ _ x) = x

main = do
    let dummyfile = "../tmp"
    -- how to create the dummy data if need be:
    --writeFile dummyfile "Purchase \"foo\" 1.1 3.14\n\nPurchase \"foo\" 1.2 3.15\n"

    input <- readFile dummyfile
    let inputlines = lines input
        fmtd :: [Purchase] -- necessary specialization for `read`
        fmtd = map read (filter (\x -> length x > 0) inputlines)
    putStrLn $ "formatted: " ++ show fmtd

    -- TODO: sort by date, then group by date
