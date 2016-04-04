data Name = Name {nameToString::String}
data Color = Color {colorToString::String}

showInfos :: Name -> Color -> String
showInfos n c = "Name:" ++ (nameToString n) ++ " Color:" ++ (colorToString c)

name :: Name
name = Name "Beach"

color :: Color
color = Color "Blue"

--correct usage
main = print (showInfos name color)

-- error caught during compilation.
--main = print (showInfos color name)



-- reflection:
-- the keyword data is similar to enum in C.
