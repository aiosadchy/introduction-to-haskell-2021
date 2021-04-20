get_repeating_lines :: [String] -> [String]
get_repeating_lines [] = []
get_repeating_lines (l:ls) | length (filter p ls) > 1 = (l:(get_repeating_lines rest))
                           | otherwise                = get_repeating_lines ls
                           where rest = filter n ls
                                 n a  = a /= l
                                 p a  = a == l


main = interact (unlines . get_repeating_lines . lines)
