ultimo :: [Int]->Int
ultimo [] = -1
ultimo [a] = a
ultimo (a:x) = ultimo x 

pegaposicao :: [Int]->Int->Int
pegaposicao (a:x) 0 = a
pegaposicao (a:x) c = pegaposicao x (c-1)--fafafape


pega :: Int->[Int]->[Int]
pega 0 (a:x) = []
pega y (a:x) = a : pega (y-1) x

retira :: Int->[Int]->[Int]
retira 0 (a:x) = (a:x)
retira y (a:x) = retira (y-1) x

soma :: [Int]->Int
soma [] = 0
soma (a:x) = a + soma x

tamanho :: [Int] ->Int
tamanho [] = 0
tamanho (a:x) = 1+tamanho x 

media :: [Int] ->Float
media l = (fromIntegral (soma l)) / (fromIntegral (tamanho l))

pegamaiores3 :: [Int] -> [Int]
pegamaiores3 [] = []
pegamaiores3 (a:x)
        |a>3 = a: pegamaiores3 x
        |otherwise = pegamaiores3 x

concatena :: [Int]->[Int]->[Int]
concatena l1 l2 = l1 ++ l2

intercala :: [Int]->[Int]->[Int]
intercala [] (b:x) = []
intercala (a:x) [] = []
intercala [] [] = []
intercala (a:z) (b:x) = a:b: intercala z x -- nao ta 100%
       

compress :: [Int]->[Int]
compress [] = []
compress (a:b:x)       
        |a == b = b : compress x
        |otherwise = a: b : compress x -- nao ta 100%


{-encode :: [Int]->(Int,Int)
encode [] = []-}

dupli :: [Int]->[Int]
dupli[] = []
dupli (a:x) = (auxdupli a )++ (dupli x)


auxdupli :: Int->[Int]
auxdupli x = [x,x]


replica ::Int->[Int]->[Int]
replica n [] = []
replica n (a:x) = (auxreplica n a)++ (replica n x)

auxreplica :: Int->Int->[Int]
auxreplica 0 a = [] 
auxreplica n a = a: (auxreplica (n-1) (a) ) 

dropEvery :: Int->[Int]->[Int]
dropEvery x [] = 0
dropEvery x









