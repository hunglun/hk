-- use haskell for studying calculus
f x = ((3 - 5 * x + x ** 2 + x ** 3  ) ** 0.5) / (x - 1)
    
main = do
  print "f x = ((3 - 5 * x + x ** 2 + x ** 3  ) ** 0.5) / (x - 1)"
  print "approaching limit of 1 from the left"
  print $map f [0.5,0.7,0.9,0.99]
  print "approaching limit of 1 from the right"
  print $map f [1.5,1.2,1.1,1.01]
  -- left limit is negative; right limit is positive
