data Name = NameConstructor {nameToString::String}
data Color = ColorConstructor {colorToString::String}

showInfos :: Name -> Color -> String
showInfos n c = "Name:" ++ (nameToString n) ++ " Color:" ++ (colorToString c)

name :: Name
name = NameConstructor "Beach"

color :: Color
color = ColorConstructor "Blue"

--correct usage
--main = print (showInfos name color)

-- error caught during compilation.
main = print (showInfos color name)
