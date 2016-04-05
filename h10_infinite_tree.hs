
data Tree a= Empty | Node a (Tree a )(Tree a) 
f :: Num a => Tree a -> [a]
f Empty = [0]
f (Node a t1 Empty) = a : f(t1)
f (Node a Empty t2) = a : f(t2)
f (Node a t1 t2) = a : (f(t1) ++ f(t2))

nullTree = Node 1 nullTree nullTree

main = print (take 3 $f nullTree)
