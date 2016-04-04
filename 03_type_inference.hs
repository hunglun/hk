-- run ghci and :type f
-- expect to see Num a => a -> a-> a
-- where a is a *type variable*

-- this example illustrate *parametric polymorphism*
-- Num is a *type class*
-- It is a set of types that share a common set of operations such as + and -.

f x y = x*x + y*y
main = print (f 2.1 3)
