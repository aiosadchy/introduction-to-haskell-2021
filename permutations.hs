-- | all_unique_permutations l - выводит все перестановки списка без дублей
all_unique_permutations :: Eq a => [a] -> [[a]]
all_unique_permutations l = unique_permutations_generator l [] size
    where size = (factorial (length l)) - 1


unique_permutations_generator :: Eq a => [a] -> [[a]] -> Integer -> [[a]]
unique_permutations_generator l known n | present = rest
                                        | otherwise = (permutation:rest)
    where present     = elem permutation known
          permutation = permutation_by_number l n
          rest        = if n > 0 then unique_permutations_generator l new_known (n - 1) else []
          new_known   = if present then known else (permutation:known)


permutation_by_number :: [a] -> Integer -> [a]
permutation_by_number l n = take_by_factoriadic_indices l i
    where i = to_factoriadic (length l) n


drop_element :: Int -> [a] -> [a]
drop_element i l = (take i l) ++ (drop (i + 1) l)


factorial :: Int -> Integer
factorial 0 = 1
factorial n = (toInteger n) * (factorial (n - 1))


to_factoriadic :: Int -> Integer -> [Int]
to_factoriadic 0 _ = []
to_factoriadic n x = (first:rest)
    where first = fromInteger (div x (factorial (n - 1)))
          rest  = to_factoriadic (n - 1) (mod x (factorial (n - 1)))


take_by_factoriadic_indices :: [a] -> [Int] -> [a]
take_by_factoriadic_indices [] []     = []
take_by_factoriadic_indices l  (i:is) = (first:rest)
    where first = l !! i
          rest  = take_by_factoriadic_indices (drop_element i l) is
