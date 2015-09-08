module LispVal where

data LispVal
	= Atom String
	| Number Integer
	| String String
	| Char Char
	| Bool Bool
	| List [LispVal]
	| DottedList [LispVal] LispVal
	deriving (Eq, Ord)

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (String contents) = show contents
showVal (Char '\n') = "#\\newline"
showVal (Char ' ') = "#\\space"
showVal (Char c) = "#\\" ++ [c]
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List xs) =
	"(" ++ unwordsList xs ++ ")"
showVal (DottedList head tail) =
	"(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where
	show = showVal
