-- list

-- this List construct restrict its members to be of the same type.

data List a = Empty | Cons a (List a) deriving (Show)
