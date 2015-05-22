import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad
import qualified Data.Char as C
import Numeric

-------------------- Parser -------------------------


instance Show Expr where show = showCommand
showCommand :: Expr -> String
-- All these are for debugging only
showCommand (NumCommand str nmb) = "NumCommand" ++ (show str) ++ " " ++ (show nmb)
showCommand (StrCommand str nmb) = "StrCommand"
showCommand (PenStateCommand str nmb) = "PenStateCommand"
showCommand (RepCommand _ exprs) = "RepCommand" ++ "{" ++ (show (map show exprs)) ++ "}"
showCommand (Number int) = show int
showCommand (String s) = s
showCommand (Comment) = "Comment"
showCommand (Variable s) = "Variabel" ++ show s
showCommand (VariableAssignment s op) = "VariableAssignment" ++ (show s) ++ " " ++ (show op)
showCommand (BinOp expr1 expr2 op) = "Bin " ++ "[" ++ (show expr1) ++ ", " ++  (show expr2) ++ ", " ++ (show op) ++  "]" 
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
         let lines =  evalAll (readExpr $ args) (TurtleState (PointData (0,0) 0 "#0000FF") [] True [])
         -- let lines =  (readExpr $ args) -- Only for debugging of parser.
         print $ lines

readExpr :: String -> [Expr]
readExpr input = case parse parseProgram "Leonardo" (map C.toLower input) of
    Left err -> [String $ "No match: " ++ show err]
    Right val -> val



-- Parses the entire thingy.
parseProgram :: Parser [Expr]
parseProgram = many parseExpr

-- Parses a single command, or expression. That name is not good.
parseExpr :: Parser Expr
parseExpr = (try parseNumCommand <|> try parseColorCommand <|> 
    try parsePenStateCommand <|> try parseRepSingle <|> 
    try parseRep <|> parseAssignment <|> parseCommentToken)

-- Parses at least 1 space, comment or newline
spaces1 :: Parser ()
spaces1 = do 
            many1 (skipMany1 (space <|> newline) <|> parseComment)
            return ()

-- Parses any number of optional spaces
spaces0 :: Parser ()
spaces0 = skipMany (space <|> newline) <|> parseComment

-- Parses a dot. Yeah.
parseDot :: Parser Char
parseDot =  char '.'

-- Parses a positive integer.
parseInt :: Parser Expr
parseInt = (liftM (Number . read) $ many1 digit)

-- Parses a negative integer.
parseNegNumber :: Parser Expr
parseNegNumber = do
                 char '-'
                 num <- parseNumber
                 return $ BinOp (Number 0) num (Operation '-') 

-- parseNumber sounds better than parseArithmetic?
parseNumber :: Parser Expr
parseNumber =  parseArithmetic

-- Parses a hexadecimal number as a String.
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

-- Parses the string 'color' followed by a hexadecimal number.
parseColorCommand :: Parser Expr
parseColorCommand = do
                cmd <- (string "color")
                spaces1
                hex <- parseHex
                spaces0
                parseDot
                spaces0
                return $ StrCommand cmd hex

-- Parses a change in pen state.
parsePenStateCommand :: Parser Expr
parsePenStateCommand = do
                cmd <- (string "up") <|> (string "down")
                spaces0
                parseDot
                spaces0
                let penState = cmd -- What now?
                return $ case penState of 
                        "up"    -> PenStateCommand cmd (Bool False)
                        "down"  -> PenStateCommand cmd (Bool True)

-- Parses a comment.
parseComment :: Parser ()
parseComment = do
                char '%'
                manyTill anyChar (void newline <|> eof)
                many space
                return ()

-- Parses a comment and returns a token. We could probably do this better but we get around some type issues real easy this way.
parseCommentToken :: Parser Expr
parseCommentToken = do
                parseComment
                return Comment

-- Parses a repetition using quotations. 
parseRep :: Parser Expr
parseRep = do
                x <- (string "rep")
                spaces1
                nmb <- parseNumber
                spaces1
                char '"'
                spaces0
                expr <- parseProgram
                spaces0
                char '"'
                spaces0
                return $ RepCommand nmb expr

-- Parses a single statement repeat.
parseRepSingle :: Parser Expr
parseRepSingle = do
                x <- (string "rep")
                spaces1
                nmb <- parseNumber
                spaces1
                expr <- parseExpr
                spaces0
                return $ RepCommand nmb [expr]

-- Parses the assignment of a variable.
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

-- Parses the usage of a variable. The value will fetched at runtime.
parseVariable :: Parser Expr
parseVariable = do
                x <- letter
                rest <- many $ (alphaNum <|> char '_')
                let name = x:rest
                return $ Variable name

-- Parses an arithmetic expression.
parseArithmetic :: Parser Expr
parseArithmetic = try parseAddition <|> try parseSubtraction <|> try parseTerm

-- Parses a term, which is a division, multiplication or a factor.
parseTerm :: Parser Expr
parseTerm       = do
                spaces0
                op <- try parseDivision <|> try parseMultiplication <|> try parseFactor
                return op

-- Parses a factor, which is a group, number, negative number or variable.
parseFactor :: Parser Expr
parseFactor = do 
                spaces0
                num <- parseGroup <|> parseInt <|> parseNegNumber <|> parseVariable
                return $ BinOp num (Number 0) (Operation '+')

-- Parses a group, which is an arithmetic expression surrounded by brackets.
parseGroup :: Parser Expr
parseGroup = do 
            char '('
            spaces0 
            ari <- parseArithmetic 
            spaces0 
            char ')'
            return ari

-- Parses addition.
parseAddition :: Parser Expr
parseAddition = do
                term <- parseTerm
                spaces0
                op <- char '+'
                spaces0
                expr <- parseArithmetic
                return $ BinOp term expr (Operation op)

-- Parses subtraction.
parseSubtraction :: Parser Expr
parseSubtraction = do
                term <- parseTerm
                spaces0
                op <- char '-'
                spaces0
                expr <- parseArithmetic
                return $ BinOp term expr (Operation op)

-- Parses multiplication.
parseMultiplication :: Parser Expr
parseMultiplication = do
                term <- parseFactor
                spaces0
                op <- char '*'
                spaces0
                expr <- parseTerm
                return $ BinOp term expr (Operation op)

-- Parses division.
parseDivision :: Parser Expr
parseDivision = do
                term <- parseFactor
                spaces0
                op <- char '/'
                spaces0
                expr <- parseTerm
                return $ BinOp term expr (Operation op)



-------------------- Evaluator -------------------------

instance Show PointData where show = showPoint
showPoint :: PointData -> String
showPoint (PointData point _ _) = (showFFloat (Just 4) (fst point) "")++ " " ++ (showFFloat (Just 4) (snd point) "")

instance Show DrawnLine where show = showLine
showLine :: DrawnLine -> String
-- Generates a line segment in SVG.
showLine (DrawnLine (PointData sPoint _ _) (PointData ePoint _ _) hexline) = 
                    "<line stroke-width=\"0.01\" x1=\"" ++ (showFFloat (Just 4) (fst sPoint) "") ++ "pt\" " ++ "y1=\"" ++ (showFFloat (Just 4) (snd sPoint) "") ++ 
                    "pt\" x2=\"" ++ (showFFloat (Just 4) (fst ePoint) "") ++ "pt\" " ++ "y2=\"" ++ (showFFloat (Just 4) (snd ePoint) "") ++ "pt\" " ++ 
                    "style=\"stroke:" ++ hexline ++ "\" />"


showLineList :: [DrawnLine] -> String
-- Generates SVG code to draw all the lines in the input list.
showLineList [] = ""
showLineList (h:t) = (show h) ++ "\n" ++ (showLineList t)

instance Show TurtleState where show = toSVG
toSVG :: TurtleState -> String
-- Generates a SVG document that draws everything contained in the TurtleState.
toSVG (TurtleState _ lines _ _) = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>" ++
                                    "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">" ++ 
                                    "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">" ++
                                    (showLineList (addToAll (reverse lines) (findSmallestValues lines (0,0)))) ++ "</svg>"

addToAll :: [DrawnLine] -> (Float, Float) -> [DrawnLine]
-- Translates all lines by the float.
addToAll [] _ = []
addToAll ((DrawnLine (PointData sp sdir shex) (PointData ep edir ehex) hex) : rest) (minx, miny) = 
    DrawnLine (PointData (fst sp - minx, snd sp - miny) sdir shex) (PointData (fst ep - minx, snd ep - miny) edir ehex) hex : addToAll rest (minx, miny)

findSmallestValues :: [DrawnLine] -> (Float, Float) -> (Float, Float)
-- Finds the smallest x and y coordinates in all the drawn lines.
findSmallestValues [] min = min
findSmallestValues ((DrawnLine (PointData sp _ _) (PointData ep _ _) _) : rest) (minx, miny) = let
        newx = min (min (fst sp) (fst ep)) minx
        newy = min (min (snd sp) (snd ep)) miny
    in
        findSmallestValues rest (newx, newy)

data PointData = PointData {
    point :: (Float, Float),
    dir :: Float,
    hex :: String
}

-- A line between two points in the given color.
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

-- Lookup for a variable.
getValue :: [(String, Float)] -> String -> Float
getValue [] _ = error "De he va int' sah bra"
getValue ((key, value):rest) maybeKey
    | maybeKey == key   = value 
    | otherwise         = getValue rest maybeKey

-- Evaluates a token and returns the proper modification of the input state.
eval :: Expr -> TurtleState -> TurtleState
eval val@(NumCommand "forw" (Number nmb)) (TurtleState (PointData point angle hex) lines penState vars) =
    let
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


eval val@(NumCommand "left" (Number angle)) (TurtleState (PointData point direction hex) lines penState vars) = 
    TurtleState (PointData point (direction - angle) hex) lines penState vars


eval val@(NumCommand "right" (Number angle)) (TurtleState (PointData point direction hex) lines penState vars) = 
    TurtleState (PointData point (direction + angle) hex) lines penState vars


eval val@(NumCommand dir binop) state = 
    eval (NumCommand dir (Number (evalNum binop state))) state

    
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

eval val@(RepCommand var exprs) state =
    eval (RepCommand (Number $ evalNum var state) exprs) state

eval val@(Comment) state = state

eval val@(VariableAssignment name (Number value)) (TurtleState pdata lines penState vars) = 
    TurtleState pdata lines penState ((name, value):vars)

eval val@(VariableAssignment name binop) (TurtleState pdata lines penState vars) = 
    TurtleState pdata lines penState ((name, (evalNum binop (TurtleState pdata lines penState vars))):vars)




evalNum :: Expr -> TurtleState -> Float
evalNum (Number n) _ = n
evalNum (Variable name) (TurtleState _ _ _ vars) = (getValue vars name)
evalNum (BinOp bin1 bin2 (Operation op)) state = (toFunction op) (evalNum bin1 state) (evalNum bin2 state)

evalAll :: [Expr] -> TurtleState -> TurtleState
evalAll [] pointData = pointData
evalAll (h:t) pointData = evalAll t (eval h pointData)

degCos :: Float -> Float
degCos a = cos (a*pi/180)

degSin :: Float -> Float
degSin a = sin (a*pi/180)

-- Converts a char to its corresponding function.
toFunction :: Char -> (Float -> Float -> Float)
toFunction '+' = (+)
toFunction '-' = (-)
toFunction '*' = (*)
toFunction '/' = (/)

-------------------- Comment -------------------------


