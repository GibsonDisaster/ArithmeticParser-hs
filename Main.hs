module Main where
  import Text.ParserCombinators.Parsec hiding (spaces)
  import Text.ParserCombinators.Parsec.Number (floating)

  data Stmt = ConstI Int
            | ConstF Float
            | Add Stmt Stmt
            | Sub Stmt Stmt
            | Div Stmt Stmt
            | Mul Stmt Stmt
            | Exp Stmt Stmt
            | Expr Stmt Char Stmt
            deriving (Show, Eq)

  constructStmt :: Stmt -> Stmt -> Char -> Stmt
  constructStmt s1 s2 o = case o of
                            '+' -> Add s1 s2
                            '-' -> Sub s1 s2
                            '*' -> Mul s1 s2
                            '/' -> Div s1 s2

  solveStmt :: Stmt -> Float
  solveStmt (ConstI i) = (fromIntegral i :: Float) / 1.0
  solveStmt (ConstF f) = f
  solveStmt (Add i i2) = (solveStmt i) + (solveStmt i2)
  solveStmt (Sub i i2) = (solveStmt i) - (solveStmt i2)
  solveStmt (Mul i i2) = (solveStmt i) * (solveStmt i2)
  solveStmt (Div i i2) = (solveStmt i) / (solveStmt i2)
  solveStmt (Expr s1 o s2) = op (solveStmt s1) (solveStmt s2)
    where
      op = case o of
            '+' -> (+)
            '-' -> (-)
            '*' -> (*)
            '/' -> (/)

  spaces :: Parser ()
  spaces = skipMany1 space

  number :: Parser Stmt
  number = do
    ds <- many1 digit
    return $ ConstI (read ds)

  float :: Parser Stmt
  float = do
    f <- floating
    return $ ConstF f

  parseParen :: Parser Stmt
  parseParen = do
    guts <- between (char '(') (char ')') mainParser
    return guts

  parseStmt :: Parser Stmt
  parseStmt = do
    st <- choice [parseParen, float, number]
    return st

  mainParser :: Parser Stmt
  mainParser = do
    num <- parseStmt
    spaces
    o <- oneOf ['+', '-', '/', '*']
    spaces
    num2 <- parseStmt
    optional spaces
    return $ constructStmt num num2 o

  main :: IO ()
  main = do
    input <- getLine
    case parse mainParser "" input of
      Left err -> putStrLn $ (show err) ++ "\nYou did something bad. Try typing it again."
      Right val -> putStrLn $ input ++ " = " ++ (show (solveStmt val))