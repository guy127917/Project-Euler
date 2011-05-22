
predicate :: Integral a => a -> Bool
predicate x = (x `mod` 3 == 0) || (x `mod` 5 == 0)

solution :: Integral a => [a] -> a
solution range = sum $ filter predicate range 

main :: IO()
main = do
	print $ solution [1..999]
