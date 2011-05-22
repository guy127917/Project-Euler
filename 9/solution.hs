import Data.List
import Data.Maybe
squares = [i*i | i <- [1..]]

-- brute force search for pythagorean triples
find_factors s = [(i,j,s) | i<-[1..s], j<-[1..s], i*i + j*j == s*s]

predicate value (a, b, c) = a + b + c == value 

findTripleFromSum value = if triple == Nothing 
	then 0
	else pythagProduct $ fromJust triple 
	where
	  triple = find (predicate value) (concat $ map find_factors [1..])
	  pythagProduct (a,b,c) = a * b * c

main = print $ findTripleFromSum 1000
