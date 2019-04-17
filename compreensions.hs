dobra :: [Int]->[Int]
dobra xs = [2*x | x<- xs]


membro :: [Int] ->Int -> Bool
membro xs v = [x|x<- xs, v == x] /= []

produtocar::[Int]->[Int]->[(Int,Int)]
produtocar l1 l2 = [(a,b) | a<- l1, b<-l2]

-- aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa

removeEspacos :: String -> String
removeEspacos l = [ x | x <-l , x /= ' ']



sings :: [[Int]] -> [Int]
sings l = [head x| x <- l, (length x) == 1]

matches :: Int -> [Int] -> [Int]
matches v l = [x|x <- l, x == v ]

elemento :: Int -> [Int] -> Bool
elemento v l	
	|(matches v l) /= [] = True
	|otherwise = False 

divisores :: Int -> [ Int]
divisores v = [ x|x <- [1..v], (mod v x) == 0] 



