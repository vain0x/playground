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
	maybe err (return . ($ args)) $ Map.lookup func primitives

primitives :: Map.Map String ([LispVal] -> LispVal)
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
		("symbol?",    Bool . typeTestAtom       . head),
		("number?",    Bool . typeTestNumber     . head),
		("string?",    Bool . typeTestString     . head),
		("character?", Bool . typeTestChar       . head),
		("boolean?",   Bool . typeTestBool       . head),
		("list?",      Bool . typeTestList       . head),
		("pair?",      Bool . typeTestDottedList . head),
		("eqv?",       Bool . equals),
		("string->symbol", symbolFromString . head),
		("symbol->string", stringFromSymbol . head)
		]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinOp op args = Number $ foldl1 op $ map unpackNum args

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

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

symbolFromString :: LispVal -> LispVal
symbolFromString (String s) = Atom s

stringFromSymbol :: LispVal -> LispVal
stringFromSymbol (Atom s) = String s
