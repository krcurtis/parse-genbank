-- Copyright 2026 Fred Hutchinson Cancer Center
--------------------------------------------------------------------------------
-- Parse feature location per Genbank format description  2004 release 140.0


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}


module ParseFeatureLocation where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex
import Text.Megaparsec.Pos
import Text.Megaparsec.Error -- for errorBundlePretty
import Data.Void -- for Void type

import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as M


import Data.Char (isSpace, isDigit)

-- import Data.List

--------------------------------------------------------------------------------
import FeatureLocation



--------------------------------------------------------------------------------

type Parser = Parsec Void T.Text


field_digits :: Parser T.Text
field_digits = do
  field <- takeWhile1P (Just "field digits") isDigit
  return field


field_angle_bracket :: Parser T.Text
field_angle_bracket = do
  field <- takeWhile1P (Just "field angle bracket") (\x -> '>' == x || '<' == x)
  return field


read_integer :: T.Text -> Int
read_integer x = read . T.unpack $ x


parse_basic_location :: Parser FeatureLocation
parse_basic_location = do
  start_imprecise <- optional (try field_angle_bracket)
  start_text <- field_digits
  _ <- string ".."
  stop_imprecise <- optional (try field_angle_bracket)
  stop_text <- field_digits

  let start = read_integer start_text
      stop = read_integer stop_text
      location = case (start_imprecise, stop_imprecise) of
                   (Nothing, Nothing) -> ContiguousSpan start stop
                   (Just _, Nothing) -> ImpreciseSpan start stop True False
                   (Just _, Just _)  -> ImpreciseSpan start stop True True
                   (Nothing, Just _)  -> ImpreciseSpan start stop False True
  return location


parse_join_location :: Parser FeatureLocation
parse_join_location = do
  _ <- string "join"
  _ <- string "("
  initial <- parse_basic_location
  rest <- many (try (string "," *> parse_basic_location))
  _ <- string ")"
  return $ LocationJoin (initial:rest)


parse_complement_location :: Parser FeatureLocation
parse_complement_location = do
  _ <- string "complement"
  _ <- string "("
  location <- parse_feature_location
  _ <- string ")"
  return $ Complement location

  


parse_feature_location :: Parser FeatureLocation
parse_feature_location = do
  results <- (try parse_basic_location)
            <|> (try parse_join_location)
            <|> (try parse_complement_location)
  return $ results



