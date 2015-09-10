{-# LANGUAGE ExistentialQuantification #-}

module Eval where

import qualified Data.Map as Map
import Control.Monad.Error
import System.IO
import LispVal

data Unpacker a = forall a. Eq a => Unpacker (LispVal -> ThrowsError a)

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _)   = return val
eval env (Atom name) = getVar env name
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, thenCl, elseCl]) =
    applyIf env pred thenCl elseCl
eval env (List (Atom "cond" : cls)) = applyCond env cls
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List (Atom "define" : args)) = applyDefine env args
eval env (List (Atom "lambda" : args)) = applyLambda env args
eval env (List (callable : args)) = do
    func <- eval env callable
    argVals <- mapM (eval env) args
    apply func argVals
eval _ badForm =
    throwError $ BadSpecialForm "Unrecognized special form" badForm

applyIf :: Env -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
applyIf env cond thenCl elseCl =
    eval env cond
    >>= liftThrows . unpackBool
    >>= \b -> eval env (if b then thenCl else elseCl)

applyCond :: Env -> [LispVal] -> IOThrowsError LispVal
applyCond env (List [Atom "else", expr] : _) =
    eval env expr
applyCond env (List [cond, expr] : xs) =
    eval env cond
    >>= liftThrows . unpackBool
    >>= \b ->
        if b
            then eval env expr
            else applyCond env xs
applyCond _ val =
    throwError $ BadSpecialForm "`cond` should take clauses each of the form (<test> <expr>) and one of <test>s must evaluate to true" (List val)

applyDefine :: Env -> [LispVal] -> IOThrowsError LispVal
applyDefine env [Atom var, val] =
    eval env val >>= defineVar env var
applyDefine env (List (Atom var : prms) : body) =
    applyLambda env (List prms : body)
    >>= defineVar env var
applyDefine env (DottedList (Atom var : prms) variadicPrm : body) =
    applyLambda env (DottedList prms variadicPrm : body)
    >>= defineVar env var
applyDefine _ args =
    throwError $ BadSpecialForm "Unrecognized `define` syntax" (List args)

applyLambda :: Env -> [LispVal] -> IOThrowsError LispVal
applyLambda env (List prms : body) =
    makeClosure env (prms, Nothing) body
applyLambda env (DottedList prms variadicPrm : body) =
    makeClosure env (prms, Just variadicPrm) body
applyLambda env (variadicPrm@(Atom _) : body) =
    makeClosure env ([],   Just variadicPrm) body
applyLambda _ args =
    throwError $ BadSpecialForm "`lambda` should take a parameter list and one or more body expressions" (List args)

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc _ func) args =
    liftThrows $ func args
apply (IOFunc _ func) args =
    func args
apply (Closure prms variadicPrm body closure) args =
    if length prms /= length args && variadicPrm == Nothing
    then throwError $ NumArgs (num prms) args
    else do
        boundEnv <-
            (liftIO $ bindVars closure $ Map.fromList $ zip prms args)
            >>= bindVarArgs variadicPrm
        evalBody boundEnv
    where
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs (Just prm) env =
            liftIO $ bindVars env $ Map.singleton prm (List remainingArgs)
        bindVarArgs Nothing env =
            return env
        remainingArgs =
            drop (length prms) args
apply val _ =
    throwError $ TypeMismatch "function" val

makeClosure :: Env -> ([LispVal], Maybe LispVal) -> [LispVal] -> IOThrowsError LispVal
makeClosure env (prms, variadicPrm) body = do
    prms'        <- mapM (liftThrows . unpackAtom) prms
    variadicPrm' <- mapM (liftThrows . unpackAtom) variadicPrm
    return $ Closure prms' variadicPrm' body env

primitives :: [(String, PrimitiveFunc)]
primitives =
        [
        ("eqv?",      untypedRelOp eqv),
        ("equals?",   untypedRelOp equals),
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
        ("string->symbol", headArg >=> symbolFromString),
        ("symbol->string", headArg >=> stringFromSymbol)
        ]

ioPrimitives :: [(String, IOFunc)]
ioPrimitives =
    [ ("apply", applyProc)
    ]

primitiveBindings :: IO Env
primitiveBindings = do
    envRef <- nullEnv
    bindVars envRef $ foldMap Map.fromList allPrimitives
    where
        allPrimitives    = [primitiveFuncs, primitiveIOFuncs]
        primitiveFuncs   = map (makeFunc PrimitiveFunc) primitives
        primitiveIOFuncs = map (makeFunc IOFunc       ) ioPrimitives
        makeFunc ctor (name, func) =
            (name, ctor name func)

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args
applyProc args = throwError $ NumArgs 1 args

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

and' :: [ThrowsError Bool] -> ThrowsError Bool
and' (x : xs) = do
    x' <- x
    y' <- and' xs
    return $ x' && y'
and' _ = return True

untypedRelOp :: (LispVal -> LispVal -> ThrowsError Bool) -> [LispVal] -> ThrowsError LispVal
untypedRelOp op args =
    if length args < 2 then
        throwError $ NumArgs 2 args
    else
        fmap Bool $ and' $ zipWith op args (tail args)

eqv :: LispVal -> LispVal -> ThrowsError Bool
eqv (Atom lhs)   (Atom rhs)    = return $ lhs == rhs
eqv (Bool lhs)   (Bool rhs)    = return $ lhs == rhs
eqv (Number lhs) (Number rhs)  = return $ lhs == rhs
eqv (String lhs) (String rhs)  = return $ lhs == rhs
eqv (DottedList xs x) (DottedList ys y) =
    let lhs = (List $ xs ++ [x]) in
    let rhs = (List $ ys ++ [y]) in
    eqv lhs rhs
eqv (List lhs) (List rhs) =
    and' $ (return $ length lhs == length rhs) : (zipWith eqv lhs rhs)
eqv _ _ = return False

equals :: LispVal -> LispVal -> ThrowsError Bool
equals lhs rhs
    |    typeTestDottedList lhs || typeTestList lhs
      || typeTestDottedList rhs || typeTestList rhs =
        do
            lhs' <- unpackList lhs
            rhs' <- unpackList rhs
            and' $ ((return $ length lhs' == length rhs') : zipWith equals lhs' rhs')
        `catchError` (const $ return False)
    | otherwise = do
        let allUnpackers = [Unpacker unpackNum, Unpacker unpackString, Unpacker unpackBool]
        primitiveEq <-
            liftM or $ forM allUnpackers $ \unpacker ->
                unpackEquals unpacker lhs rhs
        isEqv <-
            eqv lhs rhs
        return (primitiveEq || isEqv)

unpackEquals :: Unpacker a -> LispVal -> LispVal -> ThrowsError Bool
unpackEquals (Unpacker unpacker) lhs rhs =
    do  lhs' <- unpacker lhs
        rhs' <- unpacker rhs
        return $ lhs' == rhs'
    `catchError` (const $ return False)

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

symbolFromString :: LispVal -> ThrowsError LispVal
symbolFromString val = unpackString val >>= return . Atom

stringFromSymbol :: LispVal -> ThrowsError LispVal
stringFromSymbol val = unpackAtom val >>= return . String
