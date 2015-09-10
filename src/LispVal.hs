module LispVal where

import qualified Data.Map as Map
import Data.IORef
import Control.Monad.Error
import Text.Parsec (ParseError)

type PrimitiveFunc = [LispVal] -> ThrowsError LispVal

data LispVal
    = Atom String
    | Number Integer
    | String String
    | Char Char
    | Bool Bool
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | PrimitiveFunc String PrimitiveFunc

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
showVal (PrimitiveFunc name _) =
    "<primitive(" ++ name ++ ")>"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where
    show = showVal

data LispError
    = NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | UnboundVar String String
    | Default String

showError :: LispError -> String
showError (UnboundVar msg varname) = msg ++ ": " ++ varname
showError (BadSpecialForm msg form) = msg ++ ": " ++ show form
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

type Env = IORef (Map.Map String (IORef LispVal))

nullEnv :: IO Env
nullEnv = newIORef $ Map.empty

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var =
    readIORef envRef >>= return . maybe False (const True) . Map.lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe err
        (liftIO . readIORef)
        (Map.lookup var env)
    where
        err = (throwError $ UnboundVar "Getting an unbound variable: " var)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe err
        (liftIO . (flip writeIORef value))
        (Map.lookup var env)
    return value
    where
        err = throwError $ UnboundVar "Setting an unbound variable: " var

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined then do
        setVar envRef var value
        return value
    else
        liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef (Map.insert var valueRef env)
            return value

bindVars :: Env -> Map.Map String LispVal -> IO Env
bindVars envRef bindings =
    readIORef envRef >>= extendEnv bindings >>= newIORef
    where
        extendEnv bindings env =
            -- Prefer vars in bindings with the same name as ones in env.
            liftM (flip Map.union env) (sequence $ Map.map newIORef bindings)
