import Data.List (nub, sort)
import System.Environment (getArgs)

import qualified FinLib as Fin
import NotCategorized

main = do
    args <- getArgs
    let file = case args of
                 [] -> "dummy-data.txt"
                 (hd:_) -> hd

    input <- readFile file
    let pf' :: [Security]
        pf' = read input
        dates = sort (nub (concat (allDates pf')))
        pf = interpolateAllSecs pf' dates
        principals = gloo Fin.principal pf
        values = gloo Fin.value pf
        gains = gloo Fin.gain pf
        fb = gloo Fin.foobaz pf

    putStrLn $ lol pf
    putStrLn $ "Principal:\n" ++ lol principals
    putStrLn ""
    putStrLn $ "Value:\n" ++ lol values
    putStrLn ""
    putStrLn $ "Gains:\n" ++ lol gains
    putStrLn ""
    putStrLn $ "Foobaz:\n" ++ lol fb
    putStrLn ""

    let pfValue = f (map snd values)
    putStrLn $ "Pf value:\n" ++ show pfValue
    putStrLn ""

    let bb = f (map snd principals)
    putStrLn $ "Pf principal:\n" ++ show bb
    putStrLn ""

    let aa = f (map snd fb)
    putStrLn $ "Pf foobaz:\n" ++ show aa
    putStrLn ""

    let ws = weights values pfValue
    putStrLn $ "Weights:\n" ++ lol ws
    putStrLn ""

    let pcs = percent fb principals
    putStrLn $ "Percents:\n" ++ lol pcs
    putStrLn ""
