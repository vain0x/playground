module Main where

import System.IO
import System.Environment
import Control.Monad

import LispVal
import Parser
import Eval

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr =
    runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =
    evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred next action = do
    result <- next
    if pred result
        then return ()
        else action result >> until_ pred next action

runOne :: String -> IO ()
runOne expr =
    primitiveBindings >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl =
    primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> runRepl
        expr : [] -> runOne expr
