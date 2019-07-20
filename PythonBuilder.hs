import Data.List (nub, sort, intersperse)
import System.Environment (getArgs)

import qualified FinLib as Fin
import qualified Uncategorized as Uncat

main = do
    args <- getArgs
    let file = case args of
                 [] -> "dummy-data.txt"
                 (hd:_) -> hd

    input <- readFile file
    let pf' :: [Uncat.Security]
        pf' = read input
        dates = sort (nub (concat (Uncat.allDates pf')))
        pf = Uncat.interpolateAllSecs pf' dates

    let x = Uncat.gloo (Fin.normalize . (map fst)) pf

    let z = Uncat.gloo Fin.value pf
        a = Uncat.f (map snd z)
        b = Uncat.weights z a -- these are the values for the bar plot

    let banner = "# Auto-generated file"
        outfile = "pydata.py"
        xstr = "x = " ++ show x -- for line plot
        bstr = "b = " ++ show b -- for bar plot
        pf_price = Uncat.f $ Uncat.moo (map snd b) (map snd x)
        ystr = "y = " ++ show pf_price

    let all = banner : xstr : bstr : ystr : []
        all' = concat $ intersperse "\n" all

    writeFile outfile (all' ++ "\n")
