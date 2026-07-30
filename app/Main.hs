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

data CommandParameters = ExtractGeneNucleotide { egn_arg_input_genbank :: String
                                               , egn_arg_gene :: String
                                               , egn_arg_output_file :: String
                                               }
                       | ExtractLocusTagNucleotide { eltn_arg_input_genbank :: String
                                                   , eltn_arg_locus_tag :: String
                                                   , eltn_arg_output_file :: String
                                                  }
                       | ExtractProductNucleotide { epn_arg_input_genbank :: String
                                                  , epn_arg_product :: String
                                                  , epn_arg_read_prefix :: String
                                                  , epn_arg_output_file :: String
                                                  }
                       | DumpCDSNucleotide { dcn_arg_input_genbank :: String
                                           , dcn_arg_output_file :: String
                                           }
                       | DumpFasta { df_arg_input_genbank :: String
                                   , df_arg_output_file :: String
                                   }
                       | FindFeatureOverlap { ffo_arg_input_genbank :: String
                                            , ffo_arg_location :: String
                                            }
  deriving (Show)

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

product_string_parse :: IsString s => O.Parser s
product_string_parse = O.strOption
                       ( O.long "product"
                       <> O.short 'p'
                       <> O.metavar "PRODUCT"
                       <> O.help "product string"
                       )

read_prefix_parse :: IsString s => O.Parser s
read_prefix_parse = O.strOption
                      ( O.long "read-prefix"
                        <> O.short 'l'
                        <> O.metavar "LABEL"
                        <> O.help "a label to prepend to the FASTA read ID"
                      )

location_parse :: IsString s => O.Parser s
location_parse = O.strOption
                   ( O.long "location"
                   <> O.short 'n'
                   <> O.metavar "INT"
                   <> O.help "an integer location, 1-indexed"
                   )

locus_tag_parse :: IsString s => O.Parser s
locus_tag_parse = O.strOption
                       ( O.long "locus-tag"
                       <> O.metavar "LOCUS_TAG"
                       <> O.help "locus tag"
                       )


extract_gene_nucleotide_opts :: O.Parser CommandParameters
extract_gene_nucleotide_opts = ExtractGeneNucleotide <$> input_genbank_parse <*> gene_name_parse <*> output_fasta_parse

extract_locus_tag_nucleotide_opts :: O.Parser CommandParameters
extract_locus_tag_nucleotide_opts = ExtractLocusTagNucleotide <$> input_genbank_parse <*> locus_tag_parse <*> output_fasta_parse

extract_product_nucleotide_opts :: O.Parser CommandParameters
extract_product_nucleotide_opts = ExtractProductNucleotide <$> input_genbank_parse <*> product_string_parse <*> read_prefix_parse <*> output_fasta_parse

dump_cds_nucleotide_opts :: O.Parser CommandParameters
dump_cds_nucleotide_opts = DumpCDSNucleotide <$> input_genbank_parse <*> output_fasta_parse

find_feature_overlap_opts :: O.Parser CommandParameters
find_feature_overlap_opts = FindFeatureOverlap <$> input_genbank_parse <*> location_parse

dump_fasta_opts :: O.Parser CommandParameters
dump_fasta_opts = DumpFasta <$> input_genbank_parse <*> output_fasta_parse


command_parameters :: O.Parser CommandParameters
command_parameters = O.hsubparser
                   (
                      O.command "dump-cds-nucleotides"          (O.info dump_cds_nucleotide_opts          (O.progDesc "Dump all CDS regions as nucleotide sequences to FASTA file"))
                   <> O.command "dump-fasta"                    (O.info dump_fasta_opts                   (O.progDesc "Dump GenBank file's ORIGIN sequence to FASTA file"))
                   <> O.command "extract-gene-nucleotides"      (O.info extract_gene_nucleotide_opts      (O.progDesc "Extract the nucleotide sequence for the gene"))
                   <> O.command "extract-locus-tag-nucleotides" (O.info extract_locus_tag_nucleotide_opts (O.progDesc "Extract the nucleotide sequence for CDS region corresponding to the locus tag"))
                   <> O.command "extract-product-nucleotides"   (O.info extract_product_nucleotide_opts   (O.progDesc "Extract the nucleotide sequence for CDS regions matching the specified product"))
                   <> O.command "find-feature-overlap"          (O.info find_feature_overlap_opts         (O.progDesc "List any features that overlap the specified location (features that wrap around origin may be erroneously reported)"))
                   -- <> O.command "summarize"  (O.info summarize_opts    (O.progDesc "Display some summary statistics about the GenBank file"))
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

-- ----------------------------------------
run_app (ExtractLocusTagNucleotide genbank_file locus_tag output_fasta) = do
  Just g@GenBank{..} <- parse_genbank_file genbank_file
  
  -- make sure ORIGIN sequence is DNA and not amino acids
  when ("DNA" /= l_molecule_type gb_locus) $ error [i|ERROR GenBank sequence is not DNA, from file #{genbank_file}|]

  let locus_tag' = T.pack locus_tag
      matching_cds = filter (locus_tag_filter locus_tag') gb_features
      seqs = map (extract_labeled_sequence g) matching_cds

  when (0 == length seqs) $ error [i|ERROR no CDS sequences found for locus_tag #{locus_tag}|]

  putStrLn [i|Generating #{length seqs} sequences for FASTA output|]
  write_as_fasta seqs output_fasta

  putStrLn $ "DONE"
  
        
-- ----------------------------------------
run_app (ExtractProductNucleotide genbank_file product_string read_prefix output_fasta) = do
  Just g@GenBank{..} <- parse_genbank_file genbank_file

  -- make sure ORIGIN sequence is DNA and not amino acids
  when ("DNA" /= l_molecule_type gb_locus) $ error [i|ERROR GenBank sequence is not DNA, from file #{genbank_file}|]
  
  let product' = T.pack product_string
      seq_label = T.pack read_prefix
      matching_cds = filter (product_filter product') gb_features
      seqs = map (extract_labeled_sequence g) matching_cds
      seqs' = map (\(x,y) -> (seq_label <> "|" <> x, y)) seqs

  when (0 == length seqs') $ error [i|ERROR no sequences found for product #{product'}|]

  putStrLn [i|Generating #{length seqs'} sequences for FASTA output|]
  write_as_fasta seqs' output_fasta

  putStrLn $ "DONE"
  
-- ----------------------------------------
run_app (DumpCDSNucleotide genbank_file output_fasta) = do
  Just g@GenBank{..} <- parse_genbank_file genbank_file

  -- make sure ORIGIN sequence is DNA and not amino acids
  when ("DNA" /= l_molecule_type gb_locus) $ error [i|ERROR GenBank sequence is not DNA, from file #{genbank_file}|]

  let matching_cds = filter cds_filter gb_features
      seqs = map (extract_labeled_sequence g) matching_cds

  when (0 == length seqs) $ error [i|ERROR no CDS sequences found|]

  putStrLn [i|Generating #{length seqs} sequences for FASTA output|]
  write_as_fasta seqs output_fasta

  putStrLn $ "DONE"

-- ----------------------------------------
run_app (DumpFasta genbank_file output_fasta) = do
  --Just GenBank{..} <- parse_genbank_file genbank_file
  Just xs <- read_all_genbank_file genbank_file

  
  let bioseqs = map (\g -> (gb_version g, T.toUpper . gb_origin_sequence $ g)) xs
  write_as_fasta bioseqs output_fasta

  putStrLn $ "DONE"


-- ----------------------------------------
run_app (FindFeatureOverlap genbank_file location_string) = do
  Just GenBank{..} <- parse_genbank_file genbank_file

  let index = read location_string  -- read as single integer not a genbank location string
      region_loci = (index,index)
      features = filter (is_within_feature region_loci) gb_features

  when (0 == length features) $ putStrLn [i|INFO no features found for location #{index}|]

  forM_ features $ \Feature{..} -> do
    putStrLn [i|Feature #{f_feature_key} #{location_to_region f_location}|]
    forM_ f_qualifiers $ \Qualifier{..} -> do
      putStrLn [i|  #{q_name} #{q_value}|]

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


read_all_genbank_file :: String -> IO (Maybe [GenBank])
read_all_genbank_file filename = do
  text <- T.readFile filename
  -- T.putStrLn text

  let result = parse (some parse_genbank <* eof) filename text
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

product_filter :: T.Text -> Feature -> Bool
product_filter p f@Feature{..} | "CDS" == f_feature_key && has_qualifier f "product" p = True
product_filter _ _ = False


cds_filter :: Feature -> Bool
cds_filter Feature{..} | "CDS" == f_feature_key = True
cds_filter _ = False

locus_tag_filter :: T.Text -> Feature -> Bool
locus_tag_filter locus_tag f@Feature{..} | "CDS" == f_feature_key && has_qualifier f "locus_tag" locus_tag = True
locus_tag_filter _ _ = False



-- right now, only simple feature location info is implemented
extract_sequence :: FeatureLocation -> GenBank -> T.Text
extract_sequence (ContiguousSpan x y) GenBank{..} = T.toUpper $ T.take n_length $ T.drop n_pre gb_origin_sequence
  where
    n_pre = x - 1
    n_length = y - x + 1
extract_sequence (ImpreciseSpan x y _ _) g  = extract_sequence (ContiguousSpan x y) g
extract_sequence (Complement x) g = reverse_complement $ extract_sequence x g
extract_sequence (LocationJoin xs) g = T.concat . map (\x -> extract_sequence x g) $ xs
extract_sequence (LocationOrder _) _  = error "ERROR feature location: order construct not implemented"
extract_sequence (CleavageSite _ _) _  = error "ERROR feature location: cleavage site construct not implemented"
-- extract_sequence _ _  = error "ERROR other feature location constructs are not implemented"



extract_labeled_sequence :: GenBank -> Feature -> (T.Text, T.Text)
extract_labeled_sequence g (f@Feature{..}) = (seq_label, extract_sequence f_location g)
  where
    locus_tag = case (get_locus_tag f) of
                  Nothing -> "unknown_locus"
                  Just x -> x
    --protein_id = case (get_protein_id f) of
    --               Nothing -> "unknown_protein_id"
    --               Just x -> x
    seq_label = T.intercalate "|" [gb_version g, locus_tag]



get_locus_tag :: Feature -> Maybe T.Text
get_locus_tag Feature{..} = get_feature_qualifier_value "locus_tag" f_qualifiers

--get_protein_id :: Feature -> Maybe T.Text
--get_protein_id Feature{..} = get_feature_qualifier_value "protein_id" f_qualifiers

get_feature_qualifier_value :: T.Text -> [Qualifier] -> Maybe T.Text
get_feature_qualifier_value key xs = Map.lookup key qual_map
  where
    qual_map = Map.fromList . map unpack_qualifier $ xs



location_to_region :: FeatureLocation -> (Int, Int)
location_to_region (ContiguousSpan x y) = (start, stop)
  where
    coords = [x, y]
    start = minimum coords
    stop = maximum coords
location_to_region (ImpreciseSpan x y _ _) = (start, stop)
  where
    coords = [x, y]
    start = minimum coords
    stop = maximum coords
location_to_region (Complement x) = location_to_region x
location_to_region (LocationJoin xs) = (start, stop)  -- this might not work so well with locations that wrap around the end of the biological sequence
  where
    coords = concat [[x,y] | (x,y) <- map location_to_region xs]
    start = minimum coords
    stop = maximum coords
location_to_region _ = error "ERROR other location constructs not handeled for region boundary"
    
is_contained :: (Int, Int) -> (Int, Int) -> Bool
is_contained (outer_start, outer_stop) (inner_start, inner_stop) = start_inside && stop_inside
  where
    start_inside = (outer_start <= inner_start) && (inner_start <= outer_stop)
    stop_inside = (outer_start <= inner_stop) && (inner_stop <= outer_stop)


--is_feature_inside :: (Int, Int) -> Feature -> Bool
--is_feature_inside region_loci f = is_contained outer (location_to_region . f_location $ f)

is_within_feature :: (Int, Int) -> Feature -> Bool
is_within_feature region_loci f = is_contained (location_to_region . f_location $ f) region_loci

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
format_as_fasta (ident,bioseq) = text'
  where
    first_line = ">" <> ident
    n = 60
    fasta_lines = blockize_text n bioseq
    text = T.intercalate "\n" (first_line : fasta_lines)
    text' = text <> "\n"

write_as_fasta :: [(T.Text, T.Text)] -> String -> IO ()
write_as_fasta [] _ = error "ERROR no sequences given to write to FASTA file"
write_as_fasta (x : xs) filename = do
  let fasta_text = format_as_fasta x
  T.writeFile filename fasta_text
  forM_ xs $ \x' -> do
    let fasta_text' = format_as_fasta x'
    T.appendFile filename fasta_text'

