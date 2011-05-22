
squareOfSum list = n * n 
	where n = sum list

sumOfSquares [] = 0 
sumOfSquares (x:xs) = (x*x) + sumOfSquares xs

difference list = (squareOfSum list) - (sumOfSquares list)

main = print $ difference [1..100]
