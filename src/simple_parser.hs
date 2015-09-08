import Numeric
import qualified Data.Maybe as Maybe
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal
	= Atom String
	| List [LispVal]
	| DottedList [LispVal] LispVal
	| Number Integer
	| String String
	| Char Char
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
parseNumber = fmap Number (parseBin <|> parseOct <|> parseHex <|> parseDigit)
	where
		parseBin = do
			try $ string "#b"
			s <- many1 (oneOf "01")
			let [(num, rest)] = readBin s
			setInput rest
			return num
			
		tryBit '0' = Just 0
		tryBit '1' = Just 1
		tryBit _ = Nothing
		readBin = readInt 2 (Maybe.isJust . tryBit) (maybe 0 id . tryBit)
		
		parseOct = do
			try $ string "#o"
			s <- many1 (oneOf "01234567")
			let [(num, rest)] = readOct s
			setInput rest
			return num
			
		parseHex = do
			try $ string "#x"
			s <- many1 (digit <|> oneOf "abcdefABCDEF")
			let [(num, rest)] = readHex s
			setInput rest
			return num
			
		parseDigit = do
			optional $ string "#d"
			liftM read $ many1 digit

parseEscapeSequence = do
	char '\\'
	(	    (char '\\')
		<|> (char '\"')
		<|> (char '\'')
		<|> (char 't' >> return '\t')
		<|> (char 'n' >> return '\n')
		<|> (char 'r' >> return '\r')
		)

parseString :: Parser LispVal
parseString = do
	char '"'
	x <- many ((noneOf "\\\"") <|> parseEscapeSequence)
	char '"'
	return $ String x

parseChar :: Parser LispVal
parseChar =
	liftM Char $ between (char '\'') (char '\'') content
	where
		content = (noneOf "\\\'") <|> parseEscapeSequence

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseParenList :: Parser LispVal
parseParenList = do
	between (char '(') (char ')') parseList

parseExpr :: Parser LispVal
parseExpr =
	    parseParenList
	<|> parseNumber
	<|> parseString
	<|> parseChar
	<|> parseAtom

readExpr :: String -> String
readExpr input =
	case parse parseExpr "lisp" input of
		Left err -> "No match: " ++ show err
		Right val -> "Found value: " ++ show val

main :: IO ()
main = do
	args <- getArgs
	putStrLn $ readExpr (args !! 0)


