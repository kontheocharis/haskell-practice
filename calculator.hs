import Control.Applicative
import Data.Maybe
import Debug.Trace
import System.Environment

data BinaryExpr = Add | Subtract | Multiply | Divide | Power
  deriving (Show, Eq)

data BinaryPrec = PowerPrec | MulDivPrec | AddSubPrec
  deriving (Show, Eq)

data Expr = Binary BinaryExpr Expr Expr | Literal Double
  deriving (Show)

opsForPrec :: BinaryPrec -> [Char]
opsForPrec PowerPrec = ['^']
opsForPrec MulDivPrec = ['*', '/']
opsForPrec AddSubPrec = ['+', '-']

exprForOp :: Char -> BinaryExpr
exprForOp '+' = Add
exprForOp '-' = Subtract
exprForOp '*' = Multiply
exprForOp '/' = Divide
exprForOp '^' = Power

findFirstOpAcc :: Int -> [Char] -> BinaryPrec -> [Char] -> Maybe (Char, [Char], [Char])
findFirstOpAcc parensFound acc prec [] = Nothing
findFirstOpAcc parensFound acc prec input
  | f == '(' = findFirstOpAcc (parensFound + 1) (acc ++ [f]) prec curr
  | f == ')' = findFirstOpAcc (parensFound - 1) (acc ++ [f]) prec curr
  | parensFound == 0
      && prec == AddSubPrec
      && (null acc || let p = last acc in (p `notElem` ['0' .. '9'] && p /= ')')) =
    findFirstOpAcc parensFound (acc ++ [f]) prec curr
  | parensFound == 0 && f `elem` opsForPrec prec =
    if null acc || null curr
      then Nothing
      else Just (f, acc, curr)
  | otherwise = findFirstOpAcc parensFound (acc ++ [f]) prec curr
  where
    (f : curr) = input

findFirstOp :: BinaryPrec -> [Char] -> Maybe (Char, [Char], [Char])
findFirstOp = findFirstOpAcc 0 []

parseLiteralAcc :: [Char] -> Bool -> [Char] -> Maybe Expr
parseLiteralAcc acc foundPeriod []
  | last acc /= '.' = Just $ Literal $ read acc
  | otherwise = Nothing
parseLiteralAcc acc foundPeriod input
  | f == '-' && not foundPeriod && null acc = parseLiteralAcc (acc ++ [f]) foundPeriod (tail input)
  | f `elem` ['0' .. '9'] = parseLiteralAcc (acc ++ [f]) foundPeriod (tail input)
  | f == '.' && acc /= [] && not foundPeriod = parseLiteralAcc (acc ++ [f]) True (tail input)
  | otherwise = Nothing
  where
    f = head input

parseLiteral :: [Char] -> Maybe Expr
parseLiteral = parseLiteralAcc [] False

parseParens :: [Char] -> Maybe Expr
parseParens input
  | f == '(' && t == ')' = (parseExpr . tail . init) input
  | otherwise = Nothing
  where
    f = head input
    t = last input

parseBinary :: BinaryPrec -> [Char] -> Maybe Expr
parseBinary prec input
  | Just (op, left, right) <- findFirstOp prec input =
    case (parseExpr left, parseExpr right) of
      (Just leftExpr, Just rightExpr) -> Just (Binary (exprForOp op) leftExpr rightExpr)
      _ -> Nothing
  | otherwise = Nothing

parseExpr :: [Char] -> Maybe Expr
parseExpr input =
  parseBinary AddSubPrec input
    <|> parseBinary MulDivPrec input
    <|> parseBinary PowerPrec input
    <|> parseParens input
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
