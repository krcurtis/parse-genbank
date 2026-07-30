-- Copyright 2026 Fred Hutchinson Cancer Center
--------------------------------------------------------------------------------
-- Parse feature location per Genbank format description  2004 release 140.0


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RecordWildCards   #-}

module ParseGenBank where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex
import Text.Megaparsec.Pos
import Text.Megaparsec.Error -- for errorBundlePretty
import Data.Void -- for Void type

import Data.Functor ((<&>))
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as M


import Data.Char (isSpace, isDigit)

-- import Data.List

--------------------------------------------------------------------------------
import FeatureLocation
import qualified ParseFeatureLocation as P

data Stranded = SingleStranded | DoubleStranded | MixedStranded | UnknownStranded
  deriving (Show, Eq)


data Locus = Locus { l_locus_name :: T.Text
                   , l_seq_length :: Int
                   , l_stranded_type :: Stranded
                   , l_molecule_type :: T.Text
                   , l_circular :: Bool -- True if circular, or False if linear
                   , l_division_code :: Maybe T.Text -- Prokka doesn't seem to populate this
                   , l_date_field :: T.Text
                   }
  deriving (Show, Eq)


data Qualifier = Qualifier { q_name :: T.Text
                           , q_value :: T.Text  -- most of the time qualifer seems to have values, so maybe default to that, and when missing, just have empty string
                           }
  deriving (Show, Eq)


data Feature = Feature { f_feature_key :: T.Text
                       , f_location :: FeatureLocation
                       , f_qualifiers :: [Qualifier]
                       }
  deriving (Show, Eq)


data Reference = Reference { r_ref_number :: Int
                           , r_base_range :: (Int, Int)
                           , r_authors :: Maybe T.Text -- there's a small fraction of genbank files with no authors but has consortium
                           , r_consortium :: Maybe T.Text
                           , r_title :: T.Text
                           , r_journal :: T.Text
                           , r_medline :: Maybe T.Text
                           , r_pmid :: Maybe T.Text
                           , r_remark :: Maybe T.Text
                           }
  deriving (Show, Eq)


data GenBank = GenBank { gb_locus :: Locus
                       , gb_definition :: T.Text
                       , gb_accession :: T.Text
                       , gb_version :: T.Text
                       , gb_dblink :: Maybe T.Text
                       , gb_keywords :: T.Text
                       , gb_segment :: Maybe T.Text
                       , gb_source :: T.Text
                       , gb_organism :: T.Text
                       , gb_references :: [Reference]
                       , gb_comment :: Maybe T.Text
                       , gb_features :: [Feature]
                       , gb_origin_sequence :: T.Text
                       }
  deriving (Show, Eq)


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

spaces :: Parser T.Text
spaces = do
  field <- takeWhile1P (Just "spaces") (== ' ')
  return field


field_string :: Parser T.Text
field_string = do
  field <- takeWhile1P (Just "field string") (/= ' ')
  return field


field_space_string :: Parser T.Text
field_space_string = do
  field <- takeWhile1P (Just "field space string") (/= '\n')
  return field


field_empty_string :: Parser T.Text
field_empty_string = do
  _ <- lookAhead (string "\n")
  return ""


field_key :: Parser T.Text
field_key = do
  field <- takeWhile1P (Just "field key") (\x -> '=' /= x && '\n' /= x)
  return field


field_quoted :: Parser T.Text
field_quoted = do
  field <- takeWhile1P (Just "field quoted") (/= '"')
  return field

-- similar to field quoted but only for one line
field_long_maybe_quoted :: Parser T.Text
field_long_maybe_quoted = do
  field <- takeWhile1P (Just "field long") (\x -> '"' /= x && '\n' /= x)
  return field




read_integer :: T.Text -> Int
read_integer x = read . T.unpack $ x

full_indent :: Int -> T.Text
full_indent n = T.pack . take n . repeat $ ' '

flatten_sequence_text :: [T.Text] -> T.Text
flatten_sequence_text xs = T.concat ys
  where
    ys = map (T.replace " " "") xs

--------------------------------------------------------------------------------

parse_genbank :: Parser GenBank
parse_genbank = do
  gb_locus <- parse_locus

  gb_definition <- parse_entry "DEFINITION"
  gb_accession <- (try (parse_entry "ACCESSION")) <|> (try (parse_empty_entry "ACCESSION"))
  gb_version <- (try (parse_entry "VERSION")) <|> (try (parse_empty_entry "VERSION"))
  gb_dblink <- optional (try (parse_entry "DBLINK"))
  gb_keywords <- parse_entry "KEYWORDS"
  gb_segment <- optional (try (parse_entry "SEGMENT"))    -- SEGMENT ??
  gb_source <- parse_entry "SOURCE"
  gb_organism <- parse_entry "ORGANISM"
  gb_references <- many (try parse_reference)

  gb_comment <- optional (try (parse_entry "COMMENT"))
  _ <- string "FEATURES"
  _ <- spaces
  _ <- string "Location/Qualifiers"
  _ <- newline
  gb_features <- some (try (parse_feature))

  _ <- optional (try (parse_entry "CONTIG")) -- Is this part of the spec? Found some genbank files with this entry field
  gb_origin_sequence <- parse_origin

  _ <- string "//"
  _ <- newline

  return GenBank{..}



parse_locus :: Parser Locus
parse_locus = do
  _ <- string "LOCUS"
  _ <- spaces
  l_locus_name <- field_string
  _ <- spaces
  l_seq_length  <- fmap read_integer field_digits
  _ <- spaces
  _ <- string "bp"
  _ <- spaces
  l_stranded_type <- ((try (string "ss-")) <|> (try (string "ds-")) <|> (try (string "ms-"))  <|> (try (string "")))
                     <&> (\x -> case x of
                                  "ss-" -> SingleStranded
                                  "ds-" -> DoubleStranded
                                  "ms-" -> MixedStranded
                                  "" -> UnknownStranded
                         )
  l_molecule_type <- field_string
  _ <- spaces
  l_circular <- ((try (string "linear")) <|> (try (string "circular")))
                <&> (\x -> case x of
                             "linear" -> False
                             "circular" -> True)                
  l_division_code <- optional (try ( spaces *> field_string <* lookAhead (spaces <* parse_date_field)  ))
  _ <- spaces
  l_date_field <- parse_date_field
  _ <- newline
  return $ Locus{..}

parse_date_field :: Parser T.Text
parse_date_field = do
  ds <- field_digits
  sep1 <- string "-"
  month <- string "JAN"
            <|> string "FEB"
            <|> string "MAR"
            <|> string "APR"
            <|> string "MAY"
            <|> string "JUN"
            <|> string "JUL"
            <|> string "AUG"
            <|> string "SEP"
            <|> string "OCT"
            <|> string "NOV"
            <|> string "DEC"
  sep2 <- string "-"
  year <- field_digits
  return $ T.concat [ds, sep1, month, sep2, year]


parse_feature :: Parser Feature
parse_feature = do
  pre_space <- string "     " -- spec says feature key begins in column 6
  f_feature_key <- field_string
  post_space <- spaces

  let n_indent = T.length pre_space + T.length f_feature_key  + T.length post_space
      -- continuing_ident = full_indent n_indent

  f_location <- try P.parse_feature_location
  _ <- newline
  f_qualifiers <- some ( (try (parse_qualifier n_indent)) <|> (try (parse_empty_qualifier n_indent)) )
  return Feature{..}


parse_qualifier :: Int -> Parser Qualifier
parse_qualifier n_indent = do
  _ <- string (full_indent n_indent)
  _ <- char '/'
  q_name <- field_key
  _ <- char '='
  value <- (try (parse_quoted_multiline_field n_indent))
             <|> (try (parse_multiline_field n_indent))
  -- _ <- newline
  let q_value = case q_name of
                  "translation" -> T.replace "\n" "" (T.concat value)
                  _ -> T.replace "\n" "" $ T.intercalate " " value
  return Qualifier{..}


-- todo handle the double-quote escape sequence of  "\"\"", that is two double-quote characters in sequence
parse_quoted_multiline_field :: Int -> Parser [T.Text]
parse_quoted_multiline_field n_indent = do
  _ <- char '"'
  initial <- field_long_maybe_quoted

  let continuing_indent = full_indent n_indent
  contents <- many (try (optional newline *> string continuing_indent *> notFollowedBy (char '/') *> field_long_maybe_quoted))
  _ <- char '"'
  _ <- newline
  return $ (initial : contents)


parse_multiline_field :: Int -> Parser [T.Text]
parse_multiline_field n_indent = do
  initial <- field_space_string

  let continuing_indent = full_indent n_indent
  contents <- many (try (optional newline *> string continuing_indent *> notFollowedBy (char '/') *> field_space_string))
  _ <- newline
  return $ (initial : contents)

parse_empty_qualifier :: Int -> Parser Qualifier
parse_empty_qualifier n_indent = do
  _ <- string (full_indent n_indent)
  _ <- char '/'
  q_name <- field_key
  _ <- newline
  let q_value = ""
  return Qualifier{..}


parse_entry :: T.Text -> Parser T.Text
parse_entry entry_label = do
  pre_space <- optional spaces
  _ <- string entry_label
  post_space <- spaces

  let n_prespace = case pre_space of
                     Nothing -> 0
                     Just x -> T.length x
      n_indent = n_prespace + T.length entry_label + T.length post_space
      continuing_indent = full_indent n_indent


  initial <- field_space_string
  contents <- many (try (optional newline *> string continuing_indent *> (field_space_string <|> (string ""))))
  _ <- newline
  return $ T.replace "\n" "" $ T.intercalate " " (initial : contents)


parse_empty_entry :: T.Text -> Parser T.Text
parse_empty_entry entry_label = do
  _ <- optional spaces
  _ <- string entry_label
  _ <- optional spaces
  _ <- newline
  return ""


parse_reference :: Parser Reference
parse_reference = do
  _ <- string "REFERENCE"
  _ <- spaces
  r_ref_number <- fmap read_integer field_digits
  _ <- spaces
  _ <- string "(bases "
  start <- fmap read_integer field_digits
  _ <- string " to "
  stop <- fmap read_integer field_digits
  _ <- char ')'
  _ <- newline
  r_authors <- optional (try (parse_entry "AUTHORS"))
  r_consortium <- optional (try (parse_entry "CONSRTM"))
  r_title <- parse_entry "TITLE"
  r_journal <- parse_entry "JOURNAL"
  r_medline <- optional (try (parse_entry "MEDLINE"))
  r_pmid <- optional (try (parse_entry "PUBMED"))
  r_remark <- optional (try (parse_entry "REMARK"))

  let r_base_range = (start, stop)

  return Reference{..}


parse_origin :: Parser T.Text
parse_origin = do
  _ <- string "ORIGIN"
  _ <- spaces
  _ <- newline
  contents <- some (spaces *> field_digits *> char ' ' *> field_space_string <* newline)
  return $ flatten_sequence_text contents
