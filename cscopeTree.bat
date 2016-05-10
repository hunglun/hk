cd c:\snapshot\ice1_main_dev\LNX
c:\
runghc c:\Users\ao1\code\hk\cscopeTree.hs %1 > %1.dot 
dot -Tpng %1.dot -o %1.png
mspaint %1.png