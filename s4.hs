import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad
import qualified Data.Char as C
import Numeric

-------------------- Parser -------------------------

instance Show Expr where show = showCommand
showCommand :: Expr -> String
showCommand (NumCommand str nmb) = "NumCommand" ++ (show str) ++ " " ++ (show nmb)
showCommand (StrCommand str nmb) = "StrCommand"
showCommand (PenStateCommand str nmb) = "PenStateCommand"
showCommand (RepCommand _ exprs) = "RepCommand" ++ "{" ++ (show (map show exprs)) ++ "}"
showCommand (Number int) = show int
showCommand (String s) = s
showCommand (Comment) = "Comment"
showCommand (Variable s) = "Variabel" ++ show s
showCommand (VariableAssignment s op) = "VariableAssignment" ++ (show s) ++ " " ++ (show op)
showCommand (BinOp expr1 expr2 op) = "Bin " ++ "(" ++ (show expr1) ++ " " ++  (show expr2) ++ " " ++ (show op) ++  " )" 
showCommand (Operation opr ) = "OP " ++ (show opr) 

-- Tokens
data Expr =  
               Number Float
             | BinOp Expr Expr Expr
             | Operation Char
             | String String
             | Bool Bool
             | NumCommand String Expr
             | StrCommand String Expr
             | PenStateCommand String Expr
             | RepCommand Expr [Expr]
             | Comment
             | VariableAssignment String Expr
             | Variable String
             | Arithmetic String

main :: IO ()
main = do
         args <- getContents
         let lines = (readExpr $ args) -- (TurtleState (PointData (0,0) 0 "#0000FF") [] True [])
         print $ lines

readExpr :: String -> [Expr]
readExpr input = case parse parseProgram "Leonardo" (map C.toLower input) of
    Left err -> [String $ "No match: " ++ show err]
    Right val -> val



-- Parses the entire thingy.
parseProgram :: Parser [Expr]
parseProgram = many parseExpr

parseExpr :: Parser Expr
parseExpr = (parseNumCommand <|> parseColorCommand <|> 
    try parsePenStateCommand <|> try parseRep <|> 
    parseRepSingle <|> parseAssignment <|> parseComment)

spaces1 :: Parser ()
spaces1 = skipMany1 (space <|> newline)

spaces0 :: Parser ()
spaces0 = skipMany (space <|> newline)

parseDot :: Parser Char
parseDot =  char '.'

-- Exciting stuff. TODO: Figure it out
parseInt :: Parser Expr
parseInt = (liftM (Number . read) $ many1 digit)

parseNegNumber :: Parser Expr
parseNegNumber = do
                 char '-'
                 num <- parseNumber
                 return $ BinOp (Number 0) num (Operation '-') 

parseNumber :: Parser Expr
parseNumber =  parseArithmetic <|> parseInt <|> parseNegNumber <|> parseVariable


parseHex :: Parser Expr
parseHex = do
                 c <- char '#'
                 x <- many (oneOf "0123456789abcdef")
                 let hex = c:x
                 return $ String hex

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

parsePenStateCommand :: Parser Expr
parsePenStateCommand = do
                cmd <- (string "up") <|> (string "down")
                spaces0
                parseDot
                spaces0
                let penState = cmd
                return $ case penState of 
                        "up"    -> PenStateCommand cmd (Bool False)
                        "down"  -> PenStateCommand cmd (Bool True)

parseComment :: Parser Expr
parseComment = do
                string "%"
                many (noneOf "\n")
                spaces0
                return Comment

parseRep :: Parser Expr
parseRep = do
                x <- (string "rep")
                spaces1
                nmb <- parseNumber
                spaces1
                char '"'
                spaces0
                expr <- parseProgram
                char '"'
                spaces0
                return $ RepCommand nmb expr

parseRepSingle :: Parser Expr
parseRepSingle = do
                x <- (string "rep")
                spaces1
                nmb <- parseNumber
                spaces1
                expr <- parseExpr
                spaces0
                return $ RepCommand nmb [expr]

parseAssignment :: Parser Expr
parseAssignment = do
                x <- letter
                rest <- many $ (alphaNum <|> char '_')
                let name = x:rest
                spaces0
                char '='
                spaces0
                val <- parseNumber
                spaces0
                parseDot
                spaces0
                return $ VariableAssignment name val

parseVariable :: Parser Expr
parseVariable = do
                x <- letter
                rest <- many $ (alphaNum <|> char '_')
                let name = x:rest
                return $ Variable name

parseArithmetic :: Parser Expr
parseArithmetic = try parseAddition <|> try parseSubtraction <|> try parseTerm


parseTerm :: Parser Expr
parseTerm       = do
                op <- try parseDivision <|> try parseMultiplication <|> try parseFactor
                spaces0
                return op

parseFactor :: Parser Expr
parseFactor = do 
                num <- parseGroup <|> parseInt <|> parseNegNumber <|> parseVariable
                spaces0
                return $ BinOp num (Number 0) (Operation '+')

parseGroup :: Parser Expr
parseGroup = do 
            char '('
            spaces0 
            ari <- parseArithmetic 
            spaces0 
            char ')'
            spaces0
            return ari

parseAddition :: Parser Expr
parseAddition = do
                term <- parseTerm
                spaces0
                op <- char '+'
                spaces0
                expr <- parseArithmetic
                spaces0
                return $ BinOp term expr (Operation op)

parseSubtraction :: Parser Expr
parseSubtraction = do
                term <- parseTerm
                spaces0
                op <- char '-'
                spaces0
                expr <- parseArithmetic
                spaces0
                return $ BinOp term expr (Operation op)

parseMultiplication :: Parser Expr
parseMultiplication = do
                term <- parseFactor
                spaces0
                op <- char '*'
                spaces0
                expr <- parseTerm
                spaces0
                return $ BinOp term expr (Operation op)

parseDivision :: Parser Expr
parseDivision = do
                term <- parseFactor
                spaces0
                op <- char '/'
                spaces0
                expr <- parseTerm
                spaces0
                return $ BinOp term expr (Operation op)



-------------------- Evaluator -------------------------


instance Show PointData where show = showPoint
showPoint :: PointData -> String
showPoint (PointData point _ _) = (showFFloat (Just 4) (fst point) "")++ " " ++ (showFFloat (Just 4) (snd point) "")

instance Show DrawnLine where show = showLine
showLine :: DrawnLine -> String
showLine (DrawnLine startPoint endPoint hexline) = hexline ++ " " ++ (show startPoint) ++ " " ++ (show endPoint) 

instance Show TurtleState where show = showState
showState :: TurtleState -> String
showState (TurtleState _ lines _ _) = showLineList (reverse lines)

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
    lines :: [DrawnLine],
    penState :: Bool,
    vars :: [(String, Float)]
}

getValue :: [(String, Float)] -> String -> Float
getValue [] _ = error "De he va int' sah bra"
getValue ((key, value):rest) maybeKey
    | maybeKey == key   = value 
    | otherwise         = getValue rest key


eval :: Expr -> TurtleState -> TurtleState
eval val@(NumCommand "forw" (Number nmb)) (TurtleState (PointData point angle hex) lines penState vars) =
    let
        pdata = PointData ((fst point) + nmb * degCos angle, (snd point) + nmb * degSin angle ) angle hex
        newline = DrawnLine (PointData point angle hex) pdata hex
    in case penState of 
        True      -> TurtleState pdata (newline:lines) True vars
        otherwise -> TurtleState pdata lines False vars

eval val@(NumCommand "forw" (Variable name)) (TurtleState (PointData point angle hex) lines penState vars) =
    let
        nmb = getValue vars name
        pdata = PointData ((fst point) + nmb * degCos angle, (snd point) + nmb * degSin angle ) angle hex
        newline = DrawnLine (PointData point angle hex) pdata hex
    in case penState of 
        True      -> TurtleState pdata (newline:lines) True vars
        otherwise -> TurtleState pdata lines False vars


eval val@(NumCommand "back" (Number nmb)) (TurtleState (PointData point angle hex) lines penState vars) =
    let
        pdata = PointData ((fst point) - nmb * degCos angle, (snd point) - nmb * degSin angle ) angle hex
        newline = DrawnLine (PointData point angle hex) pdata hex
    in case penState of 
        True      -> TurtleState pdata (newline:lines) True vars
        otherwise -> TurtleState pdata lines False vars

eval val@(NumCommand "back" (Variable name)) (TurtleState (PointData point angle hex) lines penState vars) =
    let
        nmb = getValue vars name
        pdata = PointData ((fst point) - nmb * degCos angle, (snd point) - nmb * degSin angle ) angle hex
        newline = DrawnLine (PointData point angle hex) pdata hex
    in case penState of 
        True      -> TurtleState pdata (newline:lines) True vars
        otherwise -> TurtleState pdata lines False vars


eval val@(NumCommand "left" (Number angle)) (TurtleState (PointData point direction hex) lines penState vars) = 
    TurtleState (PointData point (direction + angle) hex) lines penState vars

eval val@(NumCommand "left" (Variable name)) (TurtleState (PointData point direction hex) lines penState vars) = 
    TurtleState (PointData point (direction + (getValue vars name)) hex) lines penState vars

eval val@(NumCommand "right" (Number angle)) (TurtleState (PointData point direction hex) lines penState vars) = 
    TurtleState (PointData point (direction - angle) hex) lines penState vars

eval val@(NumCommand "right" (Variable name)) (TurtleState (PointData point direction hex) lines penState vars) = 
    TurtleState (PointData point (direction - (getValue vars name)) hex) lines penState vars

    
eval val@(StrCommand "color" (String hex)) (TurtleState (PointData point direction _) lines penState vars) = 
    TurtleState (PointData point direction hex) lines penState vars

eval val@(PenStateCommand "up" (Bool bool)) (TurtleState (PointData point direction hex) lines _ vars) = 
    TurtleState (PointData point direction hex) lines bool vars

eval val@(PenStateCommand "down" (Bool bool)) (TurtleState (PointData point direction hex) lines _ vars) = 
    TurtleState (PointData point direction hex) lines bool vars

eval val@(RepCommand (Number 0) _) state = state
eval val@(RepCommand (Number n) exprs) state =
    let newState = evalAll exprs state
    in eval (RepCommand (Number (n-1)) exprs) newState

eval val@(RepCommand (Variable name) exprs) (TurtleState pdata lines hex vars) = 
    eval (RepCommand (Number (getValue vars name)) exprs) (TurtleState pdata lines hex vars)

eval val@(Comment) state = state

eval val@(VariableAssignment name (Number value)) (TurtleState pdata lines penState vars) = 
    TurtleState pdata lines penState ((name, value):vars)

evalAll :: [Expr] -> TurtleState -> TurtleState
evalAll [] pointData = pointData
evalAll (h:t) pointData = evalAll t (eval h pointData)

degCos :: Float -> Float
degCos a = cos (a*pi/180)

degSin :: Float -> Float
degSin a = sin (a*pi/180)

-------------------- Comment -------------------------


