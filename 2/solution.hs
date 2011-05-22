-- get fibonacci sequence where highest value is less than specified int
fib :: Int -> [Int]
fib n = buildFibonacci [1, 0]
	where
	 buildFibonacci s@(x1:x2:xs) = if (x1 + x2 >= n) 
		then s
	 	else buildFibonacci $ (x1+x2):s

main :: IO()
main = do
    print $ fib 4000000
    print $ sum $ filter even (fib 4000000)
