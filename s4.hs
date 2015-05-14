import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import qualified Data.Char as C

-------------------- Parser -------------------------

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

instance Show Expr where show = showCommand
showCommand :: Expr -> String
showCommand (NumCommand str nmb) = str ++ (show nmb)
showCommand (Number int) = show int
showCommand (String s) = s

-- Tokens
data Expr =  
               Number Float
             | String String
             | NumCommand String Expr
             | StrCommand String Expr

main :: IO ()
main = do
         args <- getContents
         let lines = evalAll (readExpr $ args) (TurtleState (PointData (0,0) 0 "#0000FF") [])
         print lines

readExpr :: String -> [Expr]
readExpr input = case parse parseExpr "Leonardo" (map C.toLower input) of
    Left err -> [String $ "No match: " ++ show err]
    Right val -> val

-- Parses the entire thingy. TODO: Rename
parseExpr :: Parser [Expr]
parseExpr = many (try parseNumCommand <|> parseColorCommand)

spaces1 :: Parser ()
spaces1 = skipMany1 (space <|> newline)

spaces0 :: Parser ()
spaces0 = skipMany (space <|> newline)

parseDot :: Parser Char
parseDot =  char '.'

-- Exciting stuff. TODO: Figure it out
parseNumber :: Parser Expr
parseNumber = liftM (Number . read) $ many1 digit

parseHex :: Parser Expr
parseHex = do
                 x <- many (oneOf "#0123456789abcdef")
                 return $ String x

-- Parses a move command.
parseNumCommand :: Parser Expr
parseNumCommand = do
                cmd <- (string "forw") <|> (string "back") <|> (string "left") <|> (string "right")
                spaces1
                nmb <- parseNumber
                spaces0
                parseDot
                spaces0
                return $ NumCommand cmd nmb

parseColorCommand :: Parser Expr
parseColorCommand = do
                cmd <- (string "color")
                spaces1
                hex <- parseHex
                spaces0
                parseDot
                spaces0
                return $ StrCommand cmd hex


-------------------- Evaluator -------------------------


instance Show PointData where show = showPoint
showPoint :: PointData -> String
showPoint (PointData point _ _) = (show $ fst $ point) ++ " " ++ (show $ snd point)

instance Show DrawnLine where show = showLine
showLine :: DrawnLine -> String
showLine (DrawnLine startPoint endPoint hexline) = hexline ++ " " ++ (show startPoint) ++ " " ++ (show endPoint) 

instance Show TurtleState where show = showState
showState :: TurtleState -> String
showState (TurtleState _ lines) = showLineList (reverse lines)

showLineList :: [DrawnLine] -> String
showLineList [] = ""
showLineList (h:t) = (show h) ++ "\n" ++ (showLineList t)



data PointData = PointData {
    point :: (Float, Float),
    dir :: Float,
    hex :: String
}

data DrawnLine = DrawnLine {
    startPoint :: PointData,
    endPoint :: PointData,
    hexline :: String
}

-- Contains the current state of the turtle, and all the lines that it has drawn.
data TurtleState = TurtleState {
    pointdata :: PointData,
    lines :: [DrawnLine]
}

eval :: Expr -> TurtleState -> TurtleState
eval val@(NumCommand "forw" (Number nmb)) (TurtleState (PointData point angle hex) lines) =
    let
        pdata = PointData ((fst point) + nmb * degCos angle, (snd point) + nmb * degSin angle ) angle hex
        newline = DrawnLine (PointData point angle hex) pdata hex
    in TurtleState pdata (newline:lines)

eval val@(NumCommand "back" (Number nmb)) (TurtleState (PointData point angle hex) lines ) =
    let
        pdata = PointData ((fst point) - nmb * degCos angle, (snd point) - nmb * degSin angle ) angle hex
        newline = DrawnLine (PointData point angle hex) pdata hex
    in TurtleState pdata (newline:lines)

eval val@(NumCommand "left" (Number angle)) (TurtleState (PointData point direction hex) lines) = 
    TurtleState (PointData point (direction + angle) hex) lines

eval val@(NumCommand "right" (Number angle)) (TurtleState (PointData point direction hex) lines) = 
    TurtleState (PointData point (direction - angle) hex) lines
    
eval val@(StrCommand "color" (String hex)) (TurtleState (PointData point direction _) lines) = 
    TurtleState (PointData point direction hex) lines


evalAll :: [Expr] -> TurtleState -> TurtleState
evalAll [] pointData = pointData
evalAll (h:t) pointData = evalAll t (eval h pointData)

degCos :: Float -> Float
degCos a = cos (a*pi/180)

degSin :: Float -> Float
degSin a = sin (a*pi/180)
-------------------- Comment -------------------------

-- main :: IO ()
-- main = getArgs >>= print . eval . readExpr . head

-- parseEOF :: Parser Expr
-- parseEOF = do
--             eof
--             return EOF

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
