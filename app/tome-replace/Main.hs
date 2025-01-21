module Main where

import Control.Applicative
import Data.Char
import qualified Data.Map as Map
import Data.Monoid
import Data.Yaml (ParseException, decodeFileEither)
import Parser
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO (Handle, IOMode (ReadMode, WriteMode), hGetContents, hPutStr, hPutStrLn, openFile, stderr, stdin, stdout)
import System.Random (randomRIO)
import Table

data Expr = ExprNum Int | ExprString String | ExprFunc (String, [Expr]) | ExprBool Bool
  deriving (Show)

type Func = [Expr] -> IO (Either String Expr)

-- provided functions

add :: Func
add [ExprNum a, ExprNum b] = return $ Right (ExprNum (a + b))
add _ = return $ Left "invalid arguments, usage: (+ Int Int)"

roll :: Func
roll [ExprNum sides, ExprNum modifier] = do
  result <- randomRIO (1, sides)
  return $ Right (ExprNum (result + modifier))
roll [ExprNum sides] = roll [ExprNum sides, ExprNum 0]
roll _ = return $ Left "invalid arguments, usage: (roll Int [Int])"

lessThan :: Func
lessThan [ExprNum left, ExprNum right] = return $ Right $ ExprBool (left < right)
lessThan _ = return $ Left "invalid arguments, usage: (< Int Int)"

greaterThan :: Func
greaterThan [ExprNum left, ExprNum right] = return $ Right $ ExprBool (left > right)
greaterThan _ = return $ Left "invalid arguments, usage: (> Int Int)"

ifFunc :: Func
ifFunc [ExprBool value, left, right] = return $ Right $ if value then left else right
ifFunc _ = return $ Left "invalid arguments, usage: (if Bool Expr Expr)"

tableFunc :: Func
tableFunc [ExprString name] = do
  cwd <- getCurrentDirectory
  let tablePath = cwd </> ".tome" </> "tables" </> name ++ ".yaml"
  exists <- doesFileExist tablePath
  if exists
    then do
      result <- decodeFileEither tablePath :: IO (Either ParseException Table)
      case result of
        Left err -> return $ Left $ "error parsing YAML: " ++ show err
        Right table -> do
          outcomeResult <- findOutcome table
          case outcomeResult of
            Just o -> return $ Right $ ExprString o
            Nothing -> return $ Left "failed to find outcome"
    else return $ Left "failed to locate table"
tableFunc _ = return $ Left "invalid arguments, usage: (table String)"

funcMap :: Map.Map String Func
funcMap =
  Map.fromList
    [ ("+", add),
      ("<", lessThan),
      (">", greaterThan),
      ("if", ifFunc),
      ("roll", roll),
      ("table", tableFunc)
    ]

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
    isValid = getAny . foldMap (Any .) [isAlpha, (`elem` "+-<>?")]
    inner = (\name _ args -> ExprFunc (name, args)) <$> spanP isValid <*> ws <*> sepBy ws exprP

exprBoolP :: Parser Expr
exprBoolP = (ExprBool True <$ stringP "true") <|> (ExprBool False <$ stringP "false")

exprP :: Parser Expr
exprP = exprNumP <|> exprStringP <|> exprFuncP <|> exprBoolP

wrappedExprP :: Parser Expr
wrappedExprP = charP '{' *> ws *> exprP <* ws <* charP '}'

-- expression handling

evaluate :: Expr -> IO (Either String Expr) 
evaluate (ExprFunc (name, args)) = case Map.lookup name funcMap of
  Just func -> do
    evaluated <- mapM evaluate args
    case sequence evaluated of
      Left err -> return $ Left err
      Right exprs -> func exprs
  Nothing -> return $ Left $ "no built-in function named '" ++ name ++ "'"
evaluate x = return $ Right x

toString :: Expr -> IO (Either String String)
toString (ExprNum x) = return $ Right $ show x
toString (ExprString str) = return $ Right str
toString (ExprBool value) = return $ Right $ if value then "true" else "false"
toString expr = do
  result <- evaluate expr
  case result of
    Left err -> return $ Left err
    Right expr' -> toString expr'

replaceExprs :: String -> String -> IO (Either String String)
replaceExprs ('{' : rest) accum = case runParser wrappedExprP ('{' : rest) of
  Just ("", expr) -> do
    result <- toString expr
    case result of
      Left err -> return $ Left err
      Right str -> return $ Right $ accum ++ str
  Just (rest', expr) -> do
    result <- toString expr
    case result of
      Left err -> return $ Left err
      Right str -> replaceExprs rest' (accum ++ str)
  Nothing -> return $ Left "failed to parse"
replaceExprs (x : xs) accum = replaceExprs xs (accum ++ [x])
replaceExprs [] accum = return $ Right accum

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
      case result of
        Left err -> do
          hPutStrLn stderr err
          exitWith (ExitFailure 1)
        Right str -> hPutStr (optOutput opts) str
    Nothing -> do
      hPutStrLn stderr "Usage: tome-replace [-i <input>] [-o <output>]"
      exitWith (ExitFailure 1)
