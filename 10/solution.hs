
triangleNumber n = sum [n, n-1..1]

-- returns a list of integer divisors for the number
divisors n = d ++ reverse (map (div n) d)  
	where d = filter (\i -> 0 == (rem n i)) list
		where list = [1.. floor $ sqrt $ fromIntegral n]

m value = filter (\n -> value < (length $ divisors n)) (map triangleNumber [1..])

main = print $ take 1 (m 500)
