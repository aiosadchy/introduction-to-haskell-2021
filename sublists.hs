-- | all_sublists l - получить все подсписки списка l, отсортированные по длине
all_sublists :: [a] -> [[a]]
all_sublists [] = [[]]
all_sublists l = concat (map get_sublists [0..(length l)])
    where get_sublists n = sublists n l


-- | sublists n l - получить все подсписки списка l длины n
sublists :: Int -> [a] -> [[a]]
sublists 0 _ = [[]]
sublists n l | (length l) < n = []
             | otherwise      = [(take n l)] ++ (sublists n (tail l))


-- 2 - Удалить из списка все вхождения подсписка.
--     Если после удаления в списке образовался такой же подсписок, его тоже надо удалить.
