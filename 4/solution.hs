
isPalindrome :: Int -> Bool
isPalindrome x = check $ show x
	where
	  check [] = True
	  check (x:[]) = True 
	  check x = (head x == last x) && check ( tail $ init x )

main :: IO()
main = do
	print $ maximum $ filter isPalindrome [i * j | i<- [1..999], j<- [1..999]]
