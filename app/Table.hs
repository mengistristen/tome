{-# LANGUAGE DeriveGeneric #-}

module Table where

import Control.Applicative
import Data.Yaml (FromJSON)
import GHC.Generics
import Parser
import System.Random (randomRIO)

data Range = Range (Int, Int) | Num Int

data Outcome = Outcome
  { range :: String,
    outcome :: String
  }
  deriving (Show, Generic)

instance FromJSON Outcome

data Table = Table
  { dice :: String,
    outcomes :: [Outcome]
  }
  deriving (Show, Generic)

instance FromJSON Table

rollsP :: Parser [Int]
rollsP = sepBy (charP ',') die
  where die = charP 'd' *> numP

rangeP :: Parser Range
rangeP = (\left _ right -> Range (left, right)) <$> numP <*> charP '-' <*> numP

tableNumP :: Parser Range
tableNumP = Num <$> numP

countDigits :: Int -> Int
countDigits 0 = 0
countDigits num = 1 + countDigits (div num 10)

concatInt :: Int -> Int -> Int
concatInt x y = y + (x * (10 ^ countDigits y))

determineRoll :: [Int] -> Int -> IO Int
determineRoll [] accum = return accum
determineRoll (x:xs) accum = do
  roll <- randomRIO (1, x)
  determineRoll xs (concatInt roll accum)
  
searchOutcomes :: Int -> [Outcome] -> Maybe String
searchOutcomes _ [] = Nothing
searchOutcomes roll (x:xs) = case runParser (rangeP <|> tableNumP) (range x) of
  Just (_, Range(left, right)) -> if roll >= left && roll <= right
    then Just (outcome x)
    else searchOutcomes roll xs
  Just (_, Num y) -> if roll == y 
    then Just (outcome x) 
    else searchOutcomes roll xs
  Nothing -> Nothing

findOutcome :: Table -> IO (Maybe String)
findOutcome table = do
  let rolls = runParser rollsP (dice table)
  case rolls of
    Just ("", sides) -> do
      roll <- determineRoll sides 0
      let result = searchOutcomes roll (outcomes table)
      return result
    _ -> return Nothing

