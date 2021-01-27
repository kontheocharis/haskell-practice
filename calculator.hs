import Control.Applicative
import Data.Maybe
import Debug.Trace
import System.Environment

data BinaryExpr = Add | Subtract | Multiply | Divide | Power
  deriving (Show, Eq)

data BinaryPrec = PowerPrec | MulDivPrec | AddSubPrec
  deriving (Show, Eq)

opsForPrec :: BinaryPrec -> [Char]
opsForPrec PowerPrec = ['^']
opsForPrec MulDivPrec = ['*', '/']
opsForPrec AddSubPrec = ['+', '-']

allOps = ['+', '-', '*', '/']

exprForOp :: Char -> BinaryExpr
exprForOp '+' = Add
exprForOp '-' = Subtract
exprForOp '*' = Multiply
exprForOp '/' = Divide
exprForOp '^' = Power

data FunctionType = Sin | Cos | Tan | Ln | Log | Sqrt
  deriving (Show, Eq)

funcTypeForLiteral :: [Char] -> Maybe FunctionType
funcTypeForLiteral "sin" = Just Sin
funcTypeForLiteral "cos" = Just Cos
funcTypeForLiteral "tan" = Just Tan
funcTypeForLiteral "ln" = Just Ln
funcTypeForLiteral "log" = Just Log
funcTypeForLiteral "sqrt" = Just Sqrt
funcTypeForLiteral other = Nothing

data Expr = Binary BinaryExpr Expr Expr | Literal Double | Function FunctionType Expr | Neg Expr
  deriving (Show)

findFirstOpAcc :: Int -> [Char] -> BinaryPrec -> [Char] -> Maybe (Char, [Char], [Char])
findFirstOpAcc parensFound acc prec [] = Nothing
findFirstOpAcc parensFound acc prec input@(f : curr)
  | f == '(' = findFirstOpAcc (parensFound + 1) (acc ++ [f]) prec curr
  | f == ')' = findFirstOpAcc (parensFound - 1) (acc ++ [f]) prec curr
  | parensFound == 0
      && prec == AddSubPrec
      && (null acc || last acc `elem` allOps) =
    findFirstOpAcc parensFound (acc ++ [f]) prec curr
  | parensFound == 0 && f `elem` opsForPrec prec =
    if null acc || null curr
      then Nothing
      else Just (f, acc, curr)
  | otherwise = findFirstOpAcc parensFound (acc ++ [f]) prec curr

findFirstOp :: BinaryPrec -> [Char] -> Maybe (Char, [Char], [Char])
findFirstOp = findFirstOpAcc 0 []

parseLiteralAcc :: [Char] -> Bool -> [Char] -> Maybe Expr
parseLiteralAcc acc foundPeriod []
  | last acc == '.' = Just $ Literal $ read (acc ++ "0")
  | head acc == '.' = Just $ Literal $ read ("0" ++ acc)
  | otherwise = Just $ Literal $ read acc
parseLiteralAcc acc foundPeriod input@(f : rest)
  | f `elem` ['0' .. '9'] = parseLiteralAcc (acc ++ [f]) foundPeriod rest
  | f == '.' && not foundPeriod = parseLiteralAcc (acc ++ [f]) True rest
  | otherwise = Nothing

parseLiteral :: [Char] -> Maybe Expr
parseLiteral "pi" = Just $ Literal pi
parseLiteral "e" = Just $ Literal (exp 1)
parseLiteral input = parseLiteralAcc [] False input

parseParens :: [Char] -> Maybe Expr
parseParens input@(f : rest)
  | f == '(' && t == ')' = (parseExpr . init) rest
  | otherwise = Nothing
  where
    t = last rest

parseBinary :: BinaryPrec -> [Char] -> Maybe Expr
parseBinary prec input
  | Just (op, left, right) <- findFirstOp prec input = do
    leftExpr <- parseExpr left
    rightExpr <- parseExpr right
    Just $ Binary (exprForOp op) leftExpr rightExpr
  | otherwise = Nothing

parseFunctionNameAcc :: [Char] -> [Char] -> Maybe ([Char], [Char])
parseFunctionNameAcc acc [] = Nothing
parseFunctionNameAcc acc input@(f : rest)
  | f `elem` ['a' .. 'z'] = parseFunctionNameAcc (acc ++ [f]) rest
  | null acc = Nothing
  | otherwise = Just (acc, input)

parseFunctionName :: [Char] -> Maybe ([Char], [Char])
parseFunctionName = parseFunctionNameAcc []

parseFunction :: [Char] -> Maybe Expr
parseFunction input
  | Just (functionName, rest) <- parseFunctionName input = do
    funcType <- funcTypeForLiteral functionName
    inner <- parseParens rest
    Just $ Function funcType inner
  | otherwise = Nothing

parseNeg :: [Char] -> Maybe Expr
parseNeg input@(f : rest)
  | f == '-' = parseExpr rest >>= Just . Neg
  | otherwise = Nothing

parseExpr :: [Char] -> Maybe Expr
parseExpr input =
  parseBinary AddSubPrec input
    <|> parseBinary MulDivPrec input
    <|> parseBinary PowerPrec input
    <|> parseParens input
    <|> parseFunction input
    <|> parseNeg input
    <|> parseLiteral input

parse :: [Char] -> Maybe Expr
parse [] = Just $ Literal 0
parse input = (parseExpr . filter (/= ' ')) input

evalExpr :: Expr -> Double
evalExpr (Binary Add a b) = evalExpr a + evalExpr b
evalExpr (Binary Subtract a b) = evalExpr a - evalExpr b
evalExpr (Binary Multiply a b) = evalExpr a * evalExpr b
evalExpr (Binary Divide a b) = evalExpr a / evalExpr b
evalExpr (Binary Power a b) = evalExpr a ** evalExpr b
evalExpr (Function Sin x) = sin $ evalExpr x
evalExpr (Function Cos x) = cos $ evalExpr x
evalExpr (Function Tan x) = tan $ evalExpr x
evalExpr (Function Ln x) = logBase (exp 1) $ evalExpr x
evalExpr (Function Log x) = logBase 10 $ evalExpr x
evalExpr (Function Sqrt x) = sqrt $ evalExpr x
evalExpr (Neg x) = - (evalExpr x)
evalExpr (Literal x) = x

printEval :: [Char] -> Maybe [Char]
printEval input = show . evalExpr <$> parse input

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "Must supply an argument"
    [arg] -> putStrLn (fromMaybe "Invalid argument" (printEval arg))
    _ -> error "Too many arguments"
