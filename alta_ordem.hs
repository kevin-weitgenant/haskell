times::Int->Int
times x  = 2*x

mapInt:: (Int->Int) -> [Int]->[Int]
mapInt f [] = []
mapInt f (a:x) = f a : mapInt f x

--total:: (a->b)->a->b


foldInt :: (Int -> Int -> Int) -> [Int] -> Int 	
foldInt f [a] = a
foldInt f (a:x) = f a (foldInt f x)

soma:: Int->Int->Int
soma x y = x+y


filterString :: (Char -> Bool) -> [Char] -> [Char]
filterString f [] = []
filterString f (a:x)
	|f a = a: filterString f x
	|otherwise = filterString f x



naoEspaco ::Char -> Bool
naoEspaco x = x /= ' '

soma_do_quadrado::[Int]->Int
somado_do_quadrado[] = 0
soma_do_quadrado l = foldInt soma (mapInt times l)


testa_funcao::(Int->Int)->Int->Bool
testa_funcao f 0 = True
testa_funcao f n 
	|(f n) > 0 = testa_funcao f (n-1)
	|otherwise = False

duasVezes::(Int->Int)->Int->Int
duasVezes f x = f (f x)

inter::Int->(Int->Int)->Int->Int
inter 0 f x = f x
inter n f x = f (inter (n-1) f x) 


























