
data Tree a= Empty | Cons a (Tree a )(Tree a) 
f :: Num a => Tree a -> a
f Empty = 0
f (Cons a t1 Empty) = a + f(t1)
f (Cons a Empty t2) = a + f(t2)
f (Cons a t1 t2) = a + f(t1) + f(t2)

tree = Cons 1 Empty Empty
tree2 = Cons 2 tree tree
