merge :: Ord a => [[a]] -> [a]
merge [[], [], [], []] = []
merge l = (h:(merge t)) where (h, t) = (pop_one (minhead False 0 0 (get_any l) l) l)


get_any :: [[a]] -> a
get_any ([]:xs) = get_any xs
get_any  (x:xs) = head x


-- найден
-- индекс найденного
-- индекс нашего
-- минимум
-- списки
minhead :: Ord a => Bool -> Int -> Int -> a -> [[a]] -> Int
minhead found i my val [] = i
minhead found i my val ([]:xss)     = minhead found  i (my + 1) val xss
minhead False i my val ((x:xs):xss) = minhead  True my (my + 1)   x xss
minhead True  i my val ((x:xs):xss) | x < val   = minhead True my (my + 1)   x xss
                                    | otherwise = minhead True  i (my + 1) val xss


pop_one :: Int -> [[a]] -> (a, [[a]])
pop_one 0    [x] = (head x, [(tail x)])
pop_one 0 (x:xs) = (head x, ((tail x):xs))
pop_one i (x:xs) = (e, (x:ts)) where (e, ts) = pop_one (i - 1) xs


split :: [a] -> [[a]]
split xs = ([
        take len xs,
        subsequence len len xs,
        subsequence (len * 2) len xs,
        drop (len * 3) xs
    ]) where len = (length xs + 3) `div` 4


subsequence :: Int -> Int -> [a] -> [a]
subsequence s l xs = take l (drop s xs)


mergesort :: Ord a => [a] -> [a]
mergesort  [] = []
mergesort [x] = [x]
mergesort  xs = merge (map mergesort (split xs))
