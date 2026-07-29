-- Copyright 2025 Fred Hutchinson Cancer Research Center
--------------------------------------------------------------------------------
-- Utility to help with simple tasks for working with GenBank files

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}



module Main (main) where


import qualified Options.Applicative as O
import Options.Applicative ((<|>))
import Data.Semigroup ((<>))

import Text.Megaparsec.Error -- for errorBundlePretty
import Text.Megaparsec

import qualified Data.Map as Map
import qualified Data.List as L


import System.FilePath ((</>))
import System.FilePath.Posix (takeFileName)

import Control.Monad (forM_, when) --  replicateM,)

import System.IO (openFile, IOMode(WriteMode), hClose, hPutStrLn)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Set as Set
import Data.Maybe (isNothing, isJust)
import Data.Either (partitionEithers)
import Control.Exception

import Data.String.Interpolate
import Data.String (IsString)


--------------------------------------------------------------------------------
import ParseGenBank
import FeatureLocation



--------------------------------------------------------------------------------
--- Local data structures

data CommandParameters = ExtractGeneNucleotide
  { egn_arg_input_genbank :: String
  , egn_arg_gene :: String
  , egn_arg_output_file :: String
  } deriving (Show)

--------------------------------------------------------------------------------
--- Command line argument parsing

input_genbank_parse :: IsString s => O.Parser s
input_genbank_parse = O.strOption
                      ( O.long "input-genbank"
                        <> O.short 'i'
                        <> O.metavar "FILE"
                        <> O.help "GenBank file to read"
                      )

gene_name_parse :: IsString s => O.Parser s
gene_name_parse = O.strOption
                      ( O.long "gene"
                        <> O.short 'g'
                        <> O.metavar "GENE"
                        <> O.help "gene name"
                      )

output_fasta_parse :: IsString s => O.Parser s
output_fasta_parse = O.strOption
                      ( O.long "output-fasta"
                        <> O.short 'o'
                        <> O.metavar "FILE"
                        <> O.help "FASTA file"
                      )


extract_gene_nucleotide_opts :: O.Parser CommandParameters
extract_gene_nucleotide_opts = ExtractGeneNucleotide <$> input_genbank_parse <*> gene_name_parse <*> output_fasta_parse




command_parameters :: O.Parser CommandParameters
command_parameters = O.hsubparser
                   ( O.command  "extract-gene-nucleotides" (O.info extract_gene_nucleotide_opts (O.progDesc "Extract the nucleotide sequence for the gene"))
                   -- TODO ? <> O.command "summarize"  (O.info summarize_opts    (O.progDesc "Display some summary statistics about the GenBank file"))
                   )




opts :: O.ParserInfo CommandParameters
opts = O.info (command_parameters O.<**>  O.helper) O.idm


--------------------------------------------------------------------------------
--- Main

main :: IO ()
main = O.execParser opts >>= run_app


run_app :: CommandParameters -> IO ()
run_app (ExtractGeneNucleotide genbank_file gene output_fasta) = do
  Just g@GenBank{..} <- parse_genbank_file genbank_file

  -- make sure ORIGIN sequence is DNA and not amino acids
  when ("DNA" /= l_molecule_type gb_locus) $ error [i|ERROR GenBank sequence is not DNA, from file #{genbank_file}|]

  let gene' = T.pack gene
      matching_cds = filter (gene_filter gene') gb_features
      seqs = map (extract_labeled_sequence g) matching_cds
      seqs' = map (\(x,y) -> (gene' <> "|" <> x, y)) seqs

  when (0 == length seqs') $ error [i|ERROR no sequences found for gene #{gene'}|]
  
  putStrLn [i|Generating #{length seqs'} sequences for FASTA output|]
  write_as_fasta seqs' output_fasta

  putStrLn $ "DONE"



--------------------------------------------------------------------------------
--- Utils

-- right now, assume only one genbank sequence per file, use-case is one complete genome per file
parse_genbank_file :: String -> IO (Maybe GenBank)
parse_genbank_file filename = do
  text <- T.readFile filename
  -- T.putStrLn text
  
  let result = parse parse_genbank filename text
  case result of
    Left bundle -> do
      putStr (errorBundlePretty bundle)
      return Nothing
    Right xs -> return (Just xs)


unpack_qualifier :: Qualifier -> (T.Text, T.Text)
unpack_qualifier Qualifier{..} = (q_name, q_value)


has_qualifier :: Feature -> T.Text -> T.Text -> Bool
has_qualifier Feature{..} key target =  case (Map.lookup key qual_map) of
                                          Just x | x == target -> True
                                          _ -> False
  where
    qual_map = Map.fromList . map unpack_qualifier $ f_qualifiers
  
  
gene_filter :: T.Text -> Feature -> Bool
gene_filter gene f@Feature{..} | "CDS" == f_feature_key && has_qualifier f "gene" gene = True
gene_filter _ _ = False
    

-- right now, only simple feature location info is implemented
extract_sequence :: FeatureLocation -> GenBank -> T.Text
extract_sequence (ContiguousSpan x y) GenBank{..} = T.toUpper $ T.take n_length $ T.drop n_pre gb_origin_sequence
  where
    n_pre = x - 1
    n_length = y - x + 1
extract_sequence (Complement x) g = reverse_complement $ extract_sequence x g
extract_sequence _ _  = error "ERROR other feature location constructs are not implemented"



extract_labeled_sequence :: GenBank -> Feature -> (T.Text, T.Text)
extract_labeled_sequence g (f@Feature{..}) = (label, extract_sequence f_location g)
  where
    locus_tag = case (get_locus_tag f) of
                  Nothing -> "unknown_locus"
                  Just x -> x
    protein_id = case (get_protein_id f) of
                   Nothing -> "unknown_protein_id"
                   Just x -> x
    label = T.intercalate "|" [gb_version g, locus_tag, protein_id]



get_locus_tag :: Feature -> Maybe T.Text
get_locus_tag Feature{..} = get_feature_qualifier_value "locus_tag" f_qualifiers

get_protein_id :: Feature -> Maybe T.Text
get_protein_id Feature{..} = get_feature_qualifier_value "protein_id" f_qualifiers

get_feature_qualifier_value :: T.Text -> [Qualifier] -> Maybe T.Text
get_feature_qualifier_value key xs = Map.lookup key qual_map
  where
    qual_map = Map.fromList . map unpack_qualifier $ xs

--------------------------------------------------------------------------------

complement :: Char -> Char
complement 'A' = 'T'
complement 'C' = 'G'
complement 'G' = 'C'
complement 'T' = 'A'
complement 'R' = 'Y'
complement 'Y' = 'R'
complement 'K' = 'M'
complement 'M' = 'K'
complement 'S' = 'S'
complement 'W' = 'W'
complement 'B' = 'V'
complement 'D' = 'H'
complement 'H' = 'D'
complement 'V' = 'B'
complement 'N' = 'N'
complement x = error [i|ERROR non-DNA letter #{x} does not have complement|]

reverse_complement :: T.Text -> T.Text
reverse_complement string = T.pack . map complement . reverse . T.unpack $ string


{-
search_biostring :: T.Text -> T.Text -> [(Bool,Int)]
search_biostring to_find content = matches ++ rc_matches
  where
    to_find' = encodeUtf8 . T.toUpper $ to_find
    content' = encodeUtf8 . T.toUpper $ content
    matches    = [ (False,x) | x <- indices to_find' content']
    
    rc_pattern = encodeUtf8 . reverse_complement . T.toUpper $ to_find
    rc_matches = [ (True,x) | x <- indices rc_pattern content']
-}



blockize_text :: Int -> T.Text -> [T.Text]
blockize_text n x | T.length x > n = (T.take n x) : blockize_text n (T.drop n x)
blockize_text _ x | otherwise = [x]

format_as_fasta :: (T.Text, T.Text) -> T.Text
format_as_fasta (x,"") = error [i|"ERROR In accession info #{x}, sequence cannot be empty for valid FASTA file|]
format_as_fasta (ident,seq) = text'
  where
    first_line = ">" <> ident
    n = 60
    lines = blockize_text n seq
    text = T.intercalate "\n" (first_line : lines)
    text' = text <> "\n"

write_as_fasta :: [(T.Text, T.Text)] -> String -> IO ()
write_as_fasta xs filename = do
  let fasta_text = T.concat . map format_as_fasta $ xs
  T.writeFile filename fasta_text
