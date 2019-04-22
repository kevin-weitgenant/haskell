ultimo :: [Int]->Int
ultimo [] = -1
ultimo [z] = z
ultimo (a:x) = ultimo x 

pegaposicao :: [Int]->Int->Int
pegaposicao (a:x) 0 = a
pegaposicao (a:x) c = pegaposicao x (c-1)


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
media l =  (fromIntegral (soma l)) / (fromIntegral (tamanho l))
