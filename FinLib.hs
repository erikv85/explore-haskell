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
gain s = gain' s (0, 0)
           where gain' [] _ = []
                 gain' (x:xs) mr = let newprice = fst x
                                       oldprice = fst mr
                                       pieces = snd mr
                                    in pieces * (newprice - oldprice) : (gain' xs x)

-- the functions below aren't very good/nice but they both
-- work

-- TODO: rename and improve
foo :: Num a => [(a, a)] -> [a]
foo [] = []
foo [z] = [0]
foo s = let s' = reverse s
         in reverse (foo' s')

-- TODO: rename and improve
foo' :: Num a => [(a, a)] -> [a]
foo' [] = []
foo' ((p,_):tl) = let (ps, xs) = unzip tl
                      princ = sum (map (\(r, y) -> r * y) tl)
                      pcs = sum xs
                   in (p * pcs - princ) : (foo' tl)

-- TODO: rename and improve
baz :: Num a => [(a, a)] -> [a]
baz [] = []
baz s = baz' [] s

-- TODO: rename and improve
baz' :: Num a => [(a, a)] -> [(a, a)] -> [a]
baz' _ [] = []
baz' [] (hd:tl) = 0 : baz' [hd] tl
baz' s (px:t) = let p = fst px
                    princ = sum (map (\(r, y) -> r * y) s)
                    (ps, xs) = unzip s
                    pcs = sum xs
                 in (p * pcs - princ) : (baz' (px : s) t)
