module Eval where

import qualified Data.Map as Map
import LispVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args =
	maybe (Bool False) ($ args) $ Map.lookup func primitives

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
		("eqv?",       Bool . equals)
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
