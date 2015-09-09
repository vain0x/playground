module Eval where

import qualified Data.Map as Map
import Control.Monad.Error
import LispVal

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _)   = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, thenCl, elseCl]) =
	applyIf pred thenCl elseCl
eval (List (Atom func : args)) =
	mapM eval args >>= apply func
eval badForm =
	throwError $ BadSpecialForm "Unrecognized special form" badForm

applyIf :: LispVal -> LispVal -> LispVal -> ThrowsError LispVal
applyIf pred thenCl elseCl = do
	cond <- eval pred
	if cond == Bool False
	then eval elseCl
	else eval thenCl

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
	let err = (throwError $ NotFunction "Unrecognized primitive function args" func) in
	maybe err ($ args) $ Map.lookup func primitives

primitives :: Map.Map String ([LispVal] -> ThrowsError LispVal)
primitives =
	Map.fromList
		[
		("&&",        boolBinOp (&&) True),
		("||",        boolBinOp (||) False),
		("+",         numericBinOp (+)),
		("-",         numericBinOp (-)),
		("*",         numericBinOp (*)),
		("/",         numericBinOp div),
		("mod",       numericBinOp mod),
		("quotient",  numericBinOp quot),
		("remainder", numericBinOp rem),
		("=",         numericRelOp (==)),
		("/=",        numericRelOp (/=)),
		("<",         numericRelOp (<)),
		(">",         numericRelOp (>)),
		(">=",        numericRelOp (>=)),
		("<=",        numericRelOp (<=)),
		("string=?",  strRelOp (==)),
		("string/=?", strRelOp (/=)),
		("string<?",  strRelOp (<)),
		("string>?",  strRelOp (>)),
		("string<=?", strRelOp (<=)),
		("string>=?", strRelOp (>=)),
		("car",        headArg >=> car),
		("cdr",        headArg >=> cdr),
		("symbol?",    headArg >=> (return . Bool . typeTestAtom      )),
		("number?",    headArg >=> (return . Bool . typeTestNumber    )),
		("string?",    headArg >=> (return . Bool . typeTestString    )),
		("character?", headArg >=> (return . Bool . typeTestChar      )),
		("boolean?",   headArg >=> (return . Bool . typeTestBool      )),
		("list?",      headArg >=> (return . Bool . typeTestList      )),
		("pair?",      headArg >=> (return . Bool . typeTestDottedList)),
		("eqv?",       return . Bool . equals),
		("string->symbol", headArg >=> symbolFromString),
		("symbol->string", headArg >=> stringFromSymbol)
		]

boolBinOp :: (Bool -> Bool -> Bool) -> Bool -> [LispVal] -> ThrowsError LispVal
boolBinOp op unit args =
	mapM unpackBool args >>= return . Bool . foldl op unit

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp _  [] =
	throwError $ NumArgs 2 []
numericBinOp op args =
	mapM unpackNum args >>= return . Number . foldl1 op

relOp :: (LispVal -> ThrowsError a) -> ([b] -> ThrowsError LispVal) -> (a -> a -> b) -> [LispVal] -> ThrowsError LispVal
relOp unpacker packer op args =
	if length args < 2 then
		throwError $ NumArgs 2 args
	else do
		arg_vals <- mapM unpacker args
		packer $ zipWith op arg_vals (tail arg_vals)

numericRelOp :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numericRelOp = relOp unpackNum (return . Bool . and)

strRelOp :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strRelOp = relOp unpackString (return . Bool . and)

unpackAtom :: LispVal -> ThrowsError String
unpackAtom (Atom s) = return s
unpackAtom val = throwError $ TypeMismatch "symbol" val

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool val = throwError $ TypeMismatch "boolean" val

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum val = throwError $ TypeMismatch "number" val

unpackString :: LispVal -> ThrowsError String
unpackString (String s) = return s
unpackString val = throwError $ TypeMismatch "string" val

unpackList :: LispVal -> ThrowsError [LispVal]
unpackList (List xs) = return xs
unpackList (DottedList xs x) = return $ xs ++ [x]
unpackList val = throwError $ TypeMismatch "pair" val

headArg :: [LispVal] -> ThrowsError LispVal
headArg [val] = return val
headArg args  = throwError $ NumArgs 1 args

car :: LispVal -> ThrowsError LispVal
car (List (x : xs)) = return x
car (DottedList (x : _) _) = return x
car val = throwError $ TypeMismatch "pair" val

cdr :: LispVal -> ThrowsError LispVal
cdr (List (_ : xs))            = return $ List xs
cdr (DottedList (_ : xs) tail) = return $ DottedList xs tail
cdr val = throwError $ TypeMismatch "pair" val

typeTestAtom, typeTestNumber, typeTestString,
	typeTestChar, typeTestBool, typeTestList, typeTestDottedList
	:: LispVal -> Bool

typeTestAtom       (Atom _)   = True
typeTestAtom       _          = False
typeTestNumber     (Number _) = True
typeTestNumber     _          = False
typeTestString     (String _) = True
typeTestString     _          = False
typeTestChar       (Char _) = True
typeTestChar       _        = False
typeTestBool       (Bool _) = True
typeTestBool       _        = False
typeTestList       (List _) = True
typeTestList       _        = False
typeTestDottedList (DottedList _ _) = True
typeTestDottedList _                = False

equals :: [LispVal] -> Bool
equals args =
	and $ zipWith (==) args (tail args)

symbolFromString :: LispVal -> ThrowsError LispVal
symbolFromString val = unpackString val >>= return . Atom

stringFromSymbol :: LispVal -> ThrowsError LispVal
stringFromSymbol val = unpackAtom val >>= return . String
