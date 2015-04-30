import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import qualified Data.Char as C

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseCommand :: Parser LispVal
parseCommand = do
                cmd <- (string "forw") <|> (string "back")
                return $ Atom cmd

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" (map C.toLower input) of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

parseExpr :: Parser LispVal
parseExpr = parseCommand
         -- <|> parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x
main :: IO ()
main = do
         args <- getArgs
         putStrLn (readExpr (args !! 0))
