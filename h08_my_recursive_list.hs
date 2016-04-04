infixr 5 :-:
data List a = Empty |  a :-: (List a) deriving (Eq,Ord,Show,Read)

f :: Num a => List a -> a
f Empty = 0
f (x:-:xs) = x + f(xs)
