import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import qualified Data.Char as C

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

instance Show LispVal where show = showCommand
showCommand :: LispVal -> String
showCommand (Command str nmb) = str ++ (show nmb)
showCommand (Number int) = show int
showCommand EOF = "EOF"
showCommand _ = "hej"
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | EOF
             | NewLine Char
             | Number Integer
             | String String
             | Bool Bool
             | Command String LispVal




main :: IO ()
main = do
         args <- getContents
         putStrLn (readExpr (args))

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" (map C.toLower input) of
    Left err -> "No match: " ++ show err
    Right val -> "Found value " ++ (show val)

parseExpr :: Parser LispVal
parseExpr = (parseCommand
            <|> parseNewLine) >>
            (parseEOF <|> parseExpr)
         -- <|> parseAtom
         -- <|> parseString
         -- <|> parseNumber
         --- <|> parseQuoted
         -- <|> do char '('
         --    x <- try parseList <|> parseDottedList
         --    char ')'
         --    return x

spaces :: Parser ()
spaces = skipMany1 space

parseEOF :: Parser LispVal
parseEOF = do
            eof
            return EOF
parseNewLine :: Parser LispVal
parseNewLine =  do
                chr <- (char '\n')
                return $ NewLine chr
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseCommand :: Parser LispVal
parseCommand = do
                cmd <- (string "forw") <|> (string "back")
                spaces
                nmb <- parseNumber
                return $ Command cmd nmb











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

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]
