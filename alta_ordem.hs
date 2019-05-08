
times::Int->Int
times x  = x*2

mapInt:: (Int->Int) -> [Int]->[Int]
mapInt f [] = []
mapInt f (a:x) = f a : mapInt f x

--total:: (a->b)->a->b


foldInt :: (Int -> Int -> Int) -> [Int] -> Int --revisar	
foldInt f [a] = a
foldInt f (a:x) = f a (foldInt f x)

soma:: Int->Int->Int
soma x y = x+y


filterString :: (Char -> Bool) -> [Char] -> [Char]
filterString f [] = []
filterString f (a:x)
	|f a = a: filterString f x
	|otherwise = filterString f x

naoEspaco :: Char -> Bool
naoEspaco x = x /= ' '

--quadrado::(Int->Int)->(Int->Int->Int)->[Int]->Int
--mapInt foldInt l = mapInt 





