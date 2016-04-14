addStuff:: Int -> Int
addStuff = do
  a <- (*2)
  b <- (+10)
  return (a+b)

main = do
  print $map addStuff [0,1..10]
