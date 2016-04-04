type Name = String
type Color = String

showInfos :: Name -> Color -> String
showInfos n c = "Name:" ++ n ++ " Color:" ++ c

name :: Name
name = "Beach"

color :: Color
color = "Blue"
-- main = print (showInfos name color)
main = print (showInfos color name)
