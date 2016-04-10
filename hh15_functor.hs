main = do
  print $fmap (+1) [1,2]
  let a = fmap (+1) $ Just 1
  if (a == Just 2) 
    then putStrLn "OK"
    else putStrLn "NOK"
