module Main where

import Control.Applicative
import Data.Char
import qualified Data.Map as Map
import Parser
import System.Environment (getArgs)
import System.IO (Handle, IOMode (ReadMode, WriteMode), hGetContents, hPutStr, hPutStrLn, openFile, stdin, stdout, stderr)
import System.Random (randomRIO)

data Expr = ExprNum Int | ExprString String | ExprFunc (String, [Expr]) | ExprBool Bool
  deriving (Show)

type Func = [Expr] -> IO (Maybe Expr)

-- provided functions

add :: Func
add [ExprNum a, ExprNum b] = return $ Just (ExprNum (a + b))
add _ = return Nothing

roll :: Func
roll [ExprNum sides, ExprNum modifier] = do
  result <- randomRIO (1, sides)
  return $ Just (ExprNum (result + modifier))
roll [ExprNum sides] = roll [ExprNum sides, ExprNum 0]
roll _ = return Nothing

lessThan :: Func
lessThan [ExprNum left, ExprNum right] = return $ Just $ ExprBool (left < right)
lessThan _ = return Nothing

greaterThan :: Func
greaterThan [ExprNum left, ExprNum right] = return $ Just $ ExprBool (left > right)
greaterThan _ = return Nothing

ifFunc :: Func
ifFunc [ExprBool value, left, right] = return $ Just $ if value then left else right
ifFunc _ = return Nothing

funcMap :: Map.Map String Func
funcMap = Map.fromList [("add", add), 
  ("roll", roll), 
  ("lessThan", lessThan), 
  ("greaterThan", greaterThan), 
  ("if", ifFunc)]

-- parser

exprNumP :: Parser Expr
exprNumP = ExprNum <$> numP

exprStringP :: Parser Expr
exprStringP = ExprString <$> stringLiteral
  where
    stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

exprFuncP :: Parser Expr
exprFuncP = charP '(' *> inner <* charP ')'
  where
    inner = (\name _ args -> ExprFunc (name, args)) <$> spanP isAlpha <*> ws <*> sepBy ws exprP

exprBoolP :: Parser Expr
exprBoolP = f <$> (stringP "true" <|> stringP "false")
  where
    f "true" = ExprBool True
    f "false" = ExprBool False
    f _ = undefined

exprP :: Parser Expr
exprP = exprNumP <|> exprStringP <|> exprFuncP <|> exprBoolP

wrappedExprP :: Parser Expr
wrappedExprP = charP '{' *> ws *> exprP <* ws <* charP '}'

-- expression handling

evaluate :: Expr -> IO (Maybe Expr)
evaluate (ExprFunc (name, args)) = case Map.lookup name funcMap of
  Just func -> do
    evaluated <- mapM evaluate args
    case sequence evaluated of
      Just exprs -> func exprs
      Nothing -> return Nothing
  Nothing -> return Nothing
evaluate x = return $ Just x

toString :: Expr -> IO String
toString (ExprNum x) = return $ show x
toString (ExprString str) = return str
toString (ExprBool value) = return $ if value then "true" else "false" 
toString expr = do
  result <- evaluate expr
  case result of
    Just value -> toString value
    Nothing -> return "{failed to parse}"

replaceExprs :: String -> String -> IO String
replaceExprs str accum = case runParser wrappedExprP str of
  Just (rest, expr) -> do
    evaluated <- toString expr
    replaceExprs rest (accum ++ evaluated)
  Nothing -> case str of
    (x : xs) -> replaceExprs xs (accum ++ [x])
    [] -> return accum

-- command line processing

data Options = Options
  { optInput :: Handle,
    optOutput :: Handle
  }
  deriving (Show)

defaultOptions :: Options
defaultOptions =
  Options
    { optInput = stdin,
      optOutput = stdout
    }

parseArgs :: [String] -> Options -> IO (Maybe Options)
parseArgs [] opts = return $ Just opts
parseArgs ("-i" : file : xs) opts = do
  handle <- openFile file ReadMode
  parseArgs xs opts {optInput = handle}
parseArgs ("-o" : file : xs) opts = do
  handle <- openFile file WriteMode
  parseArgs xs opts {optOutput = handle}
parseArgs (_ : _) _ = return Nothing

main :: IO ()
main = do
  args <- getArgs
  options <- parseArgs args defaultOptions
  case options of 
    Just opts -> do
      contents <- hGetContents (optInput opts)
      result <- replaceExprs contents ""
      hPutStr (optOutput opts) result
    Nothing -> do 
      hPutStrLn stderr "Usage: tome [-i <input>] [-o <output>]"
