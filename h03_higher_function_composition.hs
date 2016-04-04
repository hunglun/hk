f = sum . (map (^2)) . (filter even)
main = print (f [1,2..20])
