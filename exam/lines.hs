get_repeating_lines :: [String] -> [String]
get_repeating_lines [] = []
get_repeating_lines (l:ls) | elem l ls = (l:(get_repeating_lines rest))
                           | otherwise = get_repeating_lines ls
                           where rest = filter p ls
                                 p a  = a /= l


main = interact (unlines . get_repeating_lines . lines)
