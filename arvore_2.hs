
data Arvore a = No a [Arvore a]

arv1:: Arvore Int
arv1 = No 1 [No 3 [No 4[]], No5[No6[], No7[], No8[]]]
