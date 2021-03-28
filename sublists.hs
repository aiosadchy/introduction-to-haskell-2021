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



-- | remove_sublist_recursive l s - удалить из списка все вхождения подсписка;
--   если после удаления в списке образовался такой же подсписок, он тоже будет удалён.
remove_sublist_recursive :: Eq a => [a] -> [a] -> [a]
remove_sublist_recursive l s | found     = remove_sublist_recursive result s
                             | otherwise = l
    where (result, found) = remove_sublist l s

-- | remove_sublist l s - удалить из списка l первое вхождение подсписка s
remove_sublist :: Eq a => [a] -> [a] -> ([a], Bool)
remove_sublist l [] = (l, False)
remove_sublist [] _ = ([], False)
remove_sublist l s | (take len l) == s = (drop len l, True)
                   | otherwise         = ((first:rest), found)
    where len           = length s
          first         = head l
          (rest, found) = remove_sublist (tail l) s
