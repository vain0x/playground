module LispVal where

import Control.Monad.Error
import Text.Parsec (ParseError)

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

data LispError
    = NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | Default String

showError :: LispError -> String
showError (UnboundVar msg varname) = msg ++ ": " ++ varname
showError (BadSpecialForm msg form) = msg ++ ": " ++ show form
showError (NotFunction msg func) = msg ++ ": " ++ show func
showError (NumArgs expected found) =
    "Expected " ++ show expected  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) =
    "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) =
    "Parse error at " ++ show parseErr

instance Show LispError where
    show = showError

instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either LispError

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action $ return . show

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue _ = undefined
