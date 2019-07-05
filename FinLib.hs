module FinLib where

accumulate :: Num a => [a] -> [a]
accumulate = scanl1 (+)

prices :: Num a => [(a, a)] -> [a]
prices s = p where (p, _) = unzip s

pieces :: Num a => [(a, a)] -> [a]
pieces s = x where (_, x) = unzip s

alpha :: Num a => [(a, a)] -> [(a, a)]
alpha s = let (p, x) = unzip s
              ax = accumulate x
           in zip p ax

principal :: Num a => [(a, a)] -> [a]
principal s = accumulate (map (\(p, x) -> p * x) s)

value :: Num a => [(a, a)] -> [a]
value s = map (\(p, x) -> p * x) (alpha s)

gain :: Num a => [(a, a)] -> [a]
gain s = gain' (alpha s) (0, 0)
           where gain' [] _ = []
                 gain' (x:xs) mr = let newprice = fst x
                                       oldprice = fst mr
                                       pieces = snd mr
                                    in pieces * (newprice - oldprice) : (gain' xs x)

-- FIXME: without type specification, the inferred type of
-- `foobaz` is [(Integer, Integer)] -> [Integer], why?
foobaz :: Num a => [(a, a)] -> [a]
foobaz = accumulate . gain
