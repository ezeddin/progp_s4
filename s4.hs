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
showCommand _ = "OTHER"

-- data Dot = Dot Char
-- data Number = Number Integer
-- data Hex = Hex String
-- 
-- data Command = Command
--              | Movement Number
--              | Rotation Number
--              | Color Hex
--              | WritingMode
--              | EOF
-- 
-- data Repetition = Repetition Number [Command]
-- 
-- data Leonardo = Command | Repetition | Dot | Number | Hex

data LispVal = Atom String
             | EOF
             | Dot Char
             | NewLine Char
             | Number Integer
             | String String
             | Bool Bool
             | Command String LispVal

main :: IO ()
main = do
         args <- getContents
         putStrLn (readExpr (args))

readExpr input = case parse (spaces0 >> parseExpr) "Leonardo" (map C.toLower input) of
    Left err -> "No match: " ++ show err
    Right val -> "Found value" ++ show val


parseExpr :: Parser LispVal
parseExpr = parseCommand     
            >>  spaces0 >> (parseEOF <|> parseExpr)

spaces1 :: Parser ()
spaces1 = skipMany1 (space <|> newline)

spaces0 :: Parser ()
spaces0 = skipMany (space <|> newline)

parseEOF :: Parser LispVal
parseEOF = do
            eof
            return EOF

parseDot :: Parser LispVal
parseDot =  do
                chr <- (char '.')
                return $ Dot chr

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseCommand :: Parser LispVal
parseCommand = do
                cmd <- (string "forw") <|> (string "back")
                spaces1
                nmb <- parseNumber
                spaces0
                dot <- parseDot
                return $ Command cmd nmb


-- parseString :: Parser LispVal
-- parseString = do
--                 char '"'
--                 x <- many (noneOf "\"")
--                 char '"'
--                 return $ String x
-- 
-- parseAtom :: Parser LispVal
-- parseAtom = do
--               first <- letter <|> symbol
--               rest <- many (letter <|> digit <|> symbol)
--               let atom = first:rest
--               return $ case atom of
--                          "#t" -> Bool True
--                          "#f" -> Bool False
--                          _    -> Atom atom
-- 
-- parseQuoted :: Parser LispVal
-- parseQuoted = do
--     char '\''
--     x <- parseExpr
--     return $ List [Atom "quote", x]
