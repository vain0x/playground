module Eval where

import qualified Data.Map as Map
import Control.Monad.Error
import LispVal

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _)   = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) =
	mapM eval args >>= apply func
eval badForm =
	throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
	let err = (throwError $ NotFunction "Unrecognized primitive function args" func) in
	maybe err ($ args) $ Map.lookup func primitives

primitives :: Map.Map String ([LispVal] -> ThrowsError LispVal)
primitives =
	Map.fromList
		[
		("+",         numericBinOp (+)),
		("-",         numericBinOp (-)),
		("*",         numericBinOp (*)),
		("/",         numericBinOp div),
		("mod",       numericBinOp mod),
		("quotient",  numericBinOp quot),
		("remainder", numericBinOp rem),
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

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp _  [] =
	throwError $ NumArgs 2 []
numericBinOp op args =
	mapM unpackNum args >>= return . Number . foldl1 op

unpackAtom :: LispVal -> ThrowsError String
unpackAtom (Atom s) = return s
unpackAtom val = throwError $ TypeMismatch "symbol" val

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum val = throwError $ TypeMismatch "number" val

unpackString :: LispVal -> ThrowsError String
unpackString (String s) = return s
unpackString val = throwError $ TypeMismatch "string" val

headArg :: [LispVal] -> ThrowsError LispVal
headArg [val] = return val
headArg args  = throwError $ NumArgs 1 args

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
