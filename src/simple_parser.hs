import qualified Data.List as L
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal
	= Atom String
	| List [LispVal]
	| DottedList [LispVal] LispVal
	| Number Integer
	| String String
	| Bool Bool
	deriving (Show)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser LispVal
parseAtom = do
	first <- letter <|> symbol
	rest <- many (letter <|> symbol <|> digit)
	let atom = first : rest
	return $ case atom of
		"#t" -> Bool True
		"#f" -> Bool False
		_    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseString :: Parser LispVal
parseString = do
	char '"'
	x <- many content
	char '"'
	return $ String x
	where
		content = noneOf "\\\"" <|> (char '\\' >> esc_seq)
		esc_seq = do
			    (char '\\')
			<|> (char '\"')
			<|> (char 't' >> return '\t')
			<|> (char 'n' >> return '\n')
			<|> (char 'r' >> return '\r')

parseExpr :: Parser LispVal
parseExpr =
	    parseAtom
	<|> parseNumber
	<|> parseString

readExpr :: String -> String
readExpr input =
	case parse parseExpr "lisp" input of
		Left err -> "No match: " ++ show err
		Right val -> "Found value: " ++ show val

main :: IO ()
main = do
	args <- getArgs
	putStrLn $ readExpr (args !! 0)


