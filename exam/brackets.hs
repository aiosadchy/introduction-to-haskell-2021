matching_bracket :: Char -> Char
matching_bracket ')' = '('
matching_bracket ']' = '['
matching_bracket '}' = '{'
matching_bracket '>' = '<'


check_bracket :: String -> [Char] -> Int -> Int
check_bracket (c:cs) (o:ss) m | b == o    = bracket_balance cs ss m
                              | otherwise = -1
                              where b = matching_bracket c
check_bracket (c:cs) _ _ = -1


bracket_balance :: String -> [Char] -> Int -> Int
bracket_balance [] [] m = m
bracket_balance [] _ _ = -1
bracket_balance ('(':cs) stack m = bracket_balance cs ('(':stack) (max m (length stack + 1))
bracket_balance ('[':cs) stack m = bracket_balance cs ('[':stack) (max m (length stack + 1))
bracket_balance ('{':cs) stack m = bracket_balance cs ('{':stack) (max m (length stack + 1))
bracket_balance ('<':cs) stack m = bracket_balance cs ('<':stack) (max m (length stack + 1))
bracket_balance (')':cs) stack m = check_bracket (')':cs) stack m
bracket_balance (']':cs) stack m = check_bracket (']':cs) stack m
bracket_balance ('}':cs) stack m = check_bracket ('}':cs) stack m
bracket_balance ('>':cs) stack m = check_bracket ('>':cs) stack m
bracket_balance (c:cs) stack m = bracket_balance cs stack m


print_bracket_balance :: String -> String
print_bracket_balance input = (show (bracket_balance input [] 0)) ++ "\n"


main = interact print_bracket_balance
