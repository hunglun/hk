import System.Environment
main = do
  print "This is Hung Lun's haskell echo web server!"
  interact $ unlines . lines
