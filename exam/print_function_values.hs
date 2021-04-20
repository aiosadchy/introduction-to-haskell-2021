import System.Environment


get_function_by_name :: String -> (Double -> Double)
get_function_by_name "sin" = sin
get_function_by_name "cos" = cos
get_function_by_name "tan" = tan
get_function_by_name "log" = log
get_function_by_name "sqrt" = sqrt


calculate :: (Double -> Double) -> Double -> Double -> Double -> [Double]
calculate f a b h = map f [a, (a + h) .. b]


main = do
    args <- getArgs
    let f = get_function_by_name (args !! 0)
    let a = read (args !! 1)
    let b = read (args !! 2)
    let h = read (args !! 3)
    putStrLn (unwords (map show (calculate f a b h)))
