{-# LANGUAGE LambdaCase #-}
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

data Expr = ExprNum Int | ExprString String | ExprFunc (String, [Expr]) | ExprBool Bool | ExprEffect EffectRequest
  deriving (Show)

data EffectRequest = RollRequest Int Int | TableRequest String
  deriving (Show)

type Func = [Expr] -> Either String Expr

-- provided functions

add :: Func
add [ExprNum a, ExprNum b] = Right (ExprNum (a + b))
add _ = Left "invalid arguments, usage: (+ Int Int)"

lessThan :: Func
lessThan [ExprNum left, ExprNum right] = Right $ ExprBool (left < right)
lessThan _ = Left "invalid arguments, usage: (< Int Int)"

greaterThan :: Func
greaterThan [ExprNum left, ExprNum right] = Right $ ExprBool (left > right)
greaterThan _ = Left "invalid arguments, usage: (> Int Int)"

ifFunc :: Func
ifFunc [ExprBool value, left, right] = Right $ if value then left else right
ifFunc _ = Left "invalid arguments, usage: (if Bool Expr Expr)"

roll :: Func
roll [ExprNum sides, ExprNum modifier] = Right $ ExprEffect $ RollRequest sides modifier
roll [ExprNum sides] = roll [ExprNum sides, ExprNum 0]
roll _ = Left "invalid arguments, usage: (roll Int [Int])"

tableFunc :: Func
tableFunc [ExprString name] = Right $ ExprEffect $ TableRequest name
tableFunc _ = Left "invalid arguments, usage: (table String)"

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

interpolatedP :: Parser String
interpolatedP = concat <$> many part
  where
    part :: Parser String
    part = tryExprPart <|> textCharPart

    tryExprPart :: Parser String
    tryExprPart = Parser $ \input ->
      case runParser wrappedExprP input of
        Just (rest, expr) ->
          case toString expr of
            Left _ -> Nothing
            Right s -> Just (rest, s)
        Nothing -> Nothing

    textCharPart :: Parser String
    textCharPart = Parser $ \case
        [] -> Nothing
        (c:cs) -> Just (cs, [c])

-- expression handling

evaluate :: Expr -> IO (Either String Expr)
evaluate (ExprFunc (name, args)) = case Map.lookup name funcMap of
  Just func -> do
    evaluated <- mapM evaluate args
    case sequence evaluated of
      Left err -> return $ Left err
      Right exprs -> return $ func exprs
  Nothing -> return $ Left $ "no built-in function named '" ++ name ++ "'"
evaluate (ExprEffect (RollRequest sides modifier)) = do
  val <- randomRIO (1, sides)
  return $ Right $ ExprNum (val + modifier)
evaluate (ExprEffect (TableRequest name)) = do
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
      case runParser interpolatedP contents of
        Just("", result) -> _a
      
    Nothing -> do
      hPutStrLn stderr "Usage: tome-replace [-i <input>] [-o <output>]"
      exitWith (ExitFailure 1)
