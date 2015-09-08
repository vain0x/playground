module Main where

import System.Environment
import Control.Monad

import LispVal
import Parser
import Eval

main :: IO ()
main = do
	expr <- fmap head getArgs
	result <- return $ liftM show $ readExpr expr >>= eval
	putStrLn $ extractValue $ trapError result
