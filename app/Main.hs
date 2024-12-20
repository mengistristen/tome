module Main where

import Control.Applicative
import Data.Char
import qualified Data.Map as Map
import Parser
import System.Environment (getArgs)
import System.IO (Handle, IOMode (ReadMode, WriteMode), hGetContents, hPutStr, openFile, stdin, stdout)
import System.Random (randomRIO)

data Expr = ExprNum Int | ExprString String | ExprFunc (String, [Expr])
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

funcMap :: Map.Map String Func
funcMap = Map.fromList [("add", add), ("roll", roll)]

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
    inner = (\name _ args -> ExprFunc (name, args)) <$> spanP isAlpha <*> charP ' ' <*> sepBy (charP ' ') exprP

exprP :: Parser Expr
exprP = exprNumP <|> exprStringP <|> exprFuncP

wrappedExprP :: Parser Expr
wrappedExprP = charP '{' *> exprP <* charP '}'

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

parseArgs :: [String] -> Options -> IO Options
parseArgs [] opts = return opts
parseArgs ("-i" : file : xs) opts = do
  handle <- openFile file ReadMode
  parseArgs xs opts {optInput = handle}
parseArgs ("-o" : file : xs) opts = do
  handle <- openFile file WriteMode
  parseArgs xs opts {optOutput = handle}
parseArgs (_ : _) _ = error "print the usage here"

main :: IO ()
main = do
  args <- getArgs
  options <- parseArgs args defaultOptions
  contents <- hGetContents (optInput options)
  result <- replaceExprs contents ""
  hPutStr (optOutput options) result
