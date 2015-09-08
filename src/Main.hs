module Main where

import System.Environment

import Parser
import Eval

main :: IO ()
main = do
	expr <- fmap head getArgs
	case readExpr expr of
		Left err -> putStrLn $ err
		Right code -> print $ eval code
