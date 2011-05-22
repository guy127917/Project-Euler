
isDivis [] n = [n]
isDivis (x:xs) n = if (n `rem` x == 0) 
		   then isDivis xs n
		   else []

solution list = concat $ map (isDivis list) [n, n*2..]
	where n = head list
main = print $ head $ solution [20,19..2]
