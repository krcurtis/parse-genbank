-- Copyright 2026 Fred Hutchinson Cancer Research Center
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
import Text.Megaparsec.Char (newline)

import qualified Data.Map as Map
import qualified Data.List as L


import System.FilePath ((</>))
import System.FilePath.Posix (takeFileName)

import Control.Monad (forM_, when) --  replicateM,)

import System.IO (openFile, IOMode(WriteMode), hClose, hPutStrLn)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Set as Set
import Data.Maybe (isNothing, isJust, catMaybes)
import Data.Either (partitionEithers)
import Control.Exception

import Data.String.Interpolate
import Data.String (IsString)


--------------------------------------------------------------------------------
import ParseGenBank
import FeatureLocation
import AccessionLocationCSV
import ParseFeatureLocation
import GenomeDiagramRegion

--------------------------------------------------------------------------------
--- Local data structures

data CommandParameters = ExtractGene { eg_arg_input_genbank :: String
                                     , eg_arg_gene :: String
                                     , eg_arg_use_translation :: Bool
                                     , eg_arg_output_file :: String
                                     }
                       | ExtractLocusTag { elt_arg_input_genbank :: String
                                         , elt_arg_locus_tag :: String
                                         , elt_arg_use_translation :: Bool
                                         , elt_arg_output_file :: String
                                         }
                       | ExtractProduct { ep_arg_input_genbank :: String
                                        , ep_arg_product :: String
                                        , ep_arg_use_translation :: Bool
                                        , ep_arg_read_prefix :: String
                                        , ep_arg_output_file :: String
                                        }
                       | DumpCDS { dc_arg_input_genbank :: Maybe String
                                 , dc_arg_use_translation :: Bool
                                 , dc_arg_output_file :: String
                                 }
                       | DumpFasta { df_arg_input_genbank :: String
                                   , df_arg_output_file :: String
                                   }
                       | FilterOverlapFasta { fof_arg_input_genbank :: Maybe String
                                            , fof_arg_csv :: String
                                            , fof_arg_output_file :: String
                                            }
                       | FindFeatureOverlap { ffo_arg_input_genbank :: String
                                            , ffo_arg_location :: String
                                            }
                       | InfoTable { it_arg_input_genbank :: Maybe String
                                   , it_arg_output_tsv :: String
                                   }
                       | OperonFeatures { of_arg_input_genbank :: String
                                        , of_arg_region :: String
                                        , of_arg_output_json :: String
                                        }
                       | Search { sh_arg_input_genbank :: Maybe String
                                , sh_arg_gene :: Maybe String
                                , sh_arg_product :: Maybe String
                                , sh_arg_locus_tag :: Maybe String
                                }
  deriving (Show)


-- some information about a genbank record
data RecordInfo = RecordInfo { ri_accession :: T.Text  -- accession as used by the genbank record
                             , ri_source :: T.Text
                             , ri_bp :: Int
                             , ri_circular :: Bool
                             , ri_dblink :: T.Text  -- this field might not be present in the original genbank
                             , ri_definition :: T.Text
                             }
  deriving (Show)


--------------------------------------------------------------------------------
--- Command line argument parsing

accession_location_parse :: IsString s => O.Parser s
accession_location_parse = O.strOption
                           ( O.long "accession-csv"
                           <> O.short 'a'
                           <> O.metavar "FILE"
                           <> O.help "CSV containing columns for accession,start,end for region matching"
                           )



input_genbank_parse :: IsString s => O.Parser s
input_genbank_parse = O.strOption
                      ( O.long "input-genbank"
                        <> O.short 'i'
                        <> O.metavar "FILE"
                        <> O.help "GenBank file to read"
                      )

multi_input_genbank_parse :: IsString s => O.Parser (Maybe s)
multi_input_genbank_parse = optional $ O.strOption
                      ( O.long "input-genbank"
                        <> O.short 'i'
                        <> O.metavar "FILE"
                        <> O.help "GenBank file to read, or use stdin if not present"
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

output_json_parse :: IsString s => O.Parser s
output_json_parse = O.strOption
                      ( O.long "output-json"
                        <> O.short 'o'
                        <> O.metavar "FILE"
                        <> O.help "JSON file"
                      )


output_tsv_parse :: IsString s => O.Parser s
output_tsv_parse = O.strOption
                      ( O.long "output-tsv"
                        <> O.short 't'
                        <> O.metavar "FILE"
                        <> O.help "TSV output file"
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

region_parse :: IsString s => O.Parser s
region_parse = O.strOption
                 ( O.long "region"
                 <> O.short 'r'
                 <> O.metavar "REGION"
                 <> O.help "location of a region encoded as <start>..<end>, like 1..10"
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

translation_switch_parse :: O.Parser Bool
translation_switch_parse = O.switch
                           ( O.long "translation"
                           <> O.help "Use amino acid translation"
                           )


extract_gene_opts :: O.Parser CommandParameters
extract_gene_opts = ExtractGene <$> input_genbank_parse <*> gene_name_parse <*> translation_switch_parse <*> output_fasta_parse

extract_locus_tag_opts :: O.Parser CommandParameters
extract_locus_tag_opts = ExtractLocusTag <$> input_genbank_parse <*> locus_tag_parse <*> translation_switch_parse <*> output_fasta_parse

extract_product_opts :: O.Parser CommandParameters
extract_product_opts = ExtractProduct <$> input_genbank_parse <*> product_string_parse <*> translation_switch_parse <*> read_prefix_parse <*> output_fasta_parse

dump_cds_opts :: O.Parser CommandParameters
dump_cds_opts = DumpCDS <$> multi_input_genbank_parse <*> translation_switch_parse <*> output_fasta_parse

filter_overlap_fasta_opts :: O.Parser CommandParameters
filter_overlap_fasta_opts = FilterOverlapFasta <$> multi_input_genbank_parse <*> accession_location_parse <*> output_fasta_parse

find_feature_overlap_opts :: O.Parser CommandParameters
find_feature_overlap_opts = FindFeatureOverlap <$> input_genbank_parse <*> location_parse

dump_fasta_opts :: O.Parser CommandParameters
dump_fasta_opts = DumpFasta <$> input_genbank_parse <*> output_fasta_parse

info_table_opts :: O.Parser CommandParameters
info_table_opts = InfoTable <$> multi_input_genbank_parse <*> output_tsv_parse


operon_features_opts :: O.Parser CommandParameters
operon_features_opts = OperonFeatures <$> input_genbank_parse <*> region_parse <*> output_json_parse

search_opts :: O.Parser CommandParameters
search_opts = Search <$> multi_input_genbank_parse <*> optional gene_name_parse <*> optional product_string_parse <*> optional locus_tag_parse



command_parameters :: O.Parser CommandParameters
command_parameters = O.hsubparser
                   (
                      O.command "dump-cds"                      (O.info dump_cds_opts                     (O.progDesc "Dump all CDS region sequences to FASTA file"))
                   <> O.command "dump-fasta"                    (O.info dump_fasta_opts                   (O.progDesc "Dump GenBank file's ORIGIN sequence to FASTA file"))
                   <> O.command "extract-gene"                  (O.info extract_gene_opts                 (O.progDesc "Extract sequence for the gene CDS region"))
                   <> O.command "extract-locus-tag"             (O.info extract_locus_tag_opts            (O.progDesc "Extract sequence for CDS region corresponding to the locus tag"))
                   <> O.command "extract-product"               (O.info extract_product_opts              (O.progDesc "Extract sequence for CDS regions matching the specified product"))
                   <> O.command "filter-overlap-fasta"          (O.info filter_overlap_fasta_opts         (O.progDesc "Filter CDS regions if region overlaps, and extract the matches to a FASTA file"))
                   <> O.command "find-feature-overlap"          (O.info find_feature_overlap_opts         (O.progDesc "List any features that overlap the specified location (features that wrap around origin may be erroneously reported)"))
                   <> O.command "info-table"                    (O.info info_table_opts                   (O.progDesc "Generate TSV file with columns about the accessions, organism, and some supplementary info"))
                   <> O.command "operon-features"               (O.info operon_features_opts              (O.progDesc "Extract simplified location information of CDS features in a region, like an operon, to JSON for diagrams"))
                   <> O.command "search"                        (O.info search_opts                      (O.progDesc "Search CDS features for gene name or product string or locus tag"))
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
run_app (ExtractGene genbank_file gene use_translation output_fasta) = do
  Just g@GenBank{..} <- parse_genbank_file genbank_file

  -- make sure ORIGIN sequence is DNA and not amino acids
  when ("DNA" /= l_molecule_type gb_locus && not use_translation) $ error [i|ERROR GenBank sequence is not DNA, from file #{genbank_file}|]

  let gene' = T.pack gene
      matching_cds = filter (gene_filter gene') gb_features
      seqs = case use_translation of
               False -> map (extract_labeled_sequence g) matching_cds
               True -> catMaybes . map (extract_translation g) $ matching_cds
      seqs' = map (\(x,y) -> (gene' <> "|" <> x, y)) seqs

  when (0 == length seqs') $ error [i|ERROR no sequences found for gene #{gene'}|]

  putStrLn [i|Generating #{length seqs'} sequences for FASTA output|]
  write_as_fasta seqs' output_fasta

  putStrLn $ "DONE"

-- ----------------------------------------
run_app (ExtractLocusTag genbank_file locus_tag use_translation output_fasta) = do
  Just g@GenBank{..} <- parse_genbank_file genbank_file

  -- make sure ORIGIN sequence is DNA and not amino acids
  when ("DNA" /= l_molecule_type gb_locus && not use_translation) $ error [i|ERROR GenBank sequence is not DNA, from file #{genbank_file}|]

  let locus_tag' = T.pack locus_tag
      matching_cds = filter (locus_tag_filter locus_tag') gb_features
      seqs = case use_translation of
               False -> map (extract_labeled_sequence g) matching_cds
               True -> catMaybes . map (extract_translation g) $ matching_cds

  when (0 == length seqs) $ error [i|ERROR no CDS sequences found for locus_tag #{locus_tag}|]

  putStrLn [i|Generating #{length seqs} sequences for FASTA output|]
  write_as_fasta seqs output_fasta

  putStrLn $ "DONE"


-- ----------------------------------------
run_app (ExtractProduct genbank_file product_string use_translation read_prefix output_fasta) = do
  Just g@GenBank{..} <- parse_genbank_file genbank_file

  -- make sure ORIGIN sequence is DNA and not amino acids
  when ("DNA" /= l_molecule_type gb_locus && not use_translation) $ error [i|ERROR GenBank sequence is not DNA, from file #{genbank_file}|]

  let product' = T.pack product_string
      seq_label = T.pack read_prefix
      matching_cds = filter (product_filter product') gb_features
      seqs = case use_translation of
               False -> map (extract_labeled_sequence g) matching_cds
               True -> catMaybes . map (extract_translation g) $ matching_cds
      seqs' = map (\(x,y) -> (seq_label <> "|" <> x, y)) seqs

  when (0 == length seqs') $ error [i|ERROR no sequences found for product #{product'}|]

  putStrLn [i|Generating #{length seqs'} sequences for FASTA output|]
  write_as_fasta seqs' output_fasta

  putStrLn $ "DONE"

-- ----------------------------------------
run_app (DumpCDS genbank_file use_translation output_fasta) = do
  Just xs <- case genbank_file of
               Nothing -> read_all_genbank_from_stdin
               Just filename -> read_all_genbank_file filename

  -- Just g@GenBank{..} <- parse_genbank_file genbank_file

  -- TODO for all the genbank records, make sure ORIGIN sequence is DNA and not amino acids
  -- when ("DNA" /= l_molecule_type gb_locus) $ error [i|ERROR GenBank sequence is not DNA, from file #{genbank_file}|]
  when (not use_translation) $ mapM_ warn_non_dna xs

  let genbank_nucleotide_cds_seqs :: GenBank -> [(T.Text, T.Text)]
      genbank_nucleotide_cds_seqs g = map (extract_labeled_sequence g) matching_cds
        where matching_cds = filter cds_filter (gb_features g)

      genbank_aa_cds_seqs :: GenBank -> [(T.Text, T.Text)]
      genbank_aa_cds_seqs g = catMaybes . map (extract_translation g) $ matching_cds
        where matching_cds = filter cds_filter (gb_features g)
        
      seqs = concat $ case use_translation of
                        False -> map genbank_nucleotide_cds_seqs xs
                        True -> map genbank_aa_cds_seqs xs

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
run_app (FilterOverlapFasta genbank_file csv_file output_fasta) = do
  accession_entries <- read_accession_location_csv csv_file
  Just xs <- case genbank_file of
               Nothing -> read_all_genbank_from_stdin
               Just filename -> read_all_genbank_file filename

  let bioseqs = concat . map (filter_extract_origin_seqs accession_entries) $ xs

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


-- ----------------------------------------
run_app (InfoTable genbank_file output_tsv) = do
  --Just GenBank{..} <- parse_genbank_file genbank_file
  
  Just xs <- case genbank_file of
               Nothing -> read_all_genbank_from_stdin
               Just filename -> read_all_genbank_file filename


  let ys = map (record_line . to_record_info) xs
      header = T.intercalate "\t" [ "accession", "source", "bp", "circular", "dblink", "definition"]
      text = T.intercalate "\n" (header : ys)

  T.writeFile output_tsv text 
  T.appendFile output_tsv "\n"
  
  putStrLn $ "DONE"


-- ----------------------------------------
run_app (OperonFeatures  genbank_file encoded_region output_json) = do
  Just g@GenBank{..} <- parse_genbank_file genbank_file  

  location <- parse_encoded_region encoded_region
  
  let region = location_to_region location
      features = filter (\x -> within_region (f_location x) region) . filter cds_filter $ gb_features
      
  when (0 == length features) $ putStrLn [i|INFO no features found for location #{encoded_region}|]

  let dr_start = fst region
      dr_stop = snd region
      dr_genome = gb_source
      dr_locus_tag = "operon over" <> T.pack encoded_region
      dr_accession = gb_version
      dr_features = map to_diagram_feature features


  write_diagram_region_file output_json DiagramRegion{..}

  putStrLn $ "DONE"
    
-- ----------------------------------------
run_app (Search genbank_file gene product locus_tag) = do

  if isNothing gene && isNothing product && isNothing locus_tag then
    putStrLn $ "ERROR neither gene nor product nor locus_tag specified"
  else do
    Just xs <- case genbank_file of
                 Nothing -> read_all_genbank_from_stdin
                 Just filename -> read_all_genbank_file filename


    let semipaired_features = map (\gb -> (gb_version gb, gb_features gb)) $ xs
        paired_features = concat . map (\(x,ys) -> zip (repeat x) ys) $ semipaired_features

        features = case gene of
                     Nothing -> paired_features
                     Just gene' -> filter (gene_filter (T.pack gene') . snd) paired_features
        features' = case product of
                      Nothing -> features
                      Just product' -> filter (product_filter (T.pack product') . snd) features
        features'' = case locus_tag of
                       Nothing -> features'
                       Just locus_tag' -> filter (locus_tag_filter (T.pack locus_tag') . snd) features


    case (length  features'', length xs, gene, product, locus_tag) of
      (x,1,_,_,_) | x > 0                                -> putStrLn [i|INFO found #{x} matching CDS regions in #{gb_version . head $ xs}|]
      (x,n,_,_,_) | x > 0                                -> putStrLn [i|INFO found #{x} matching CDS regions in #{n} GenBank records|]
      (_, _, Nothing,    Nothing,       Just locus_tag') -> putStrLn [i|INFO no features found for locus_tag #{locus_tag'}|]
      (_, _, Nothing,    Just product', Nothing)         -> putStrLn [i|INFO no features found for product #{product'}|]
      (_, _, Nothing,    Just product', Just locus_tag') -> putStrLn [i|INFO no features found for product #{product'} and locus_tag #{locus_tag'}|]
      (_, _, Just gene', Nothing,       Nothing)         -> putStrLn [i|INFO no features found for gene #{gene'}|]
      (_, _, Just gene', Nothing,       Just locus_tag') -> putStrLn [i|INFO no features found for gene #{gene'} and locus_tag #{locus_tag'}|]
      (_, _, Just gene', Just product', Nothing)         -> putStrLn [i|INFO no features found for gene #{gene'} and product #{product'}|]
      (_, _, Just gene', Just product', Just locus_tag') -> putStrLn [i|INFO no features found for gene #{gene'} and product #{product'} and locus_tag #{locus_tag'}|]

    forM_ features'' $ \(accession, Feature{..}) -> do
      putStrLn [i|Feature #{f_feature_key} #{location_to_region f_location} in #{accession}|]
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

  let result = parse (some parse_genbank <* (optional newline) <* eof) filename text
  case result of
    Left bundle -> do
      putStr (errorBundlePretty bundle)
      return Nothing
    Right xs -> return (Just xs)

read_all_genbank_from_stdin :: IO (Maybe [GenBank])
read_all_genbank_from_stdin = do
  text <- T.getContents

  let result = parse (some parse_genbank <* eof) "stdin" text
  case result of
    Left bundle -> do
      putStr (errorBundlePretty bundle)
      return Nothing
    Right xs -> return (Just xs)


parse_encoded_region :: String -> IO FeatureLocation
parse_encoded_region encoded_region = do
  let result = parse parse_feature_location "encoded region" (T.pack encoded_region)
  case result of
    Left bundle -> do
      putStr (errorBundlePretty bundle)
      error "ERROR could not parse region"
    Right x -> return x



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


extract_translation :: GenBank -> Feature -> Maybe (T.Text, T.Text)
extract_translation g (f@Feature{..}) = do
  let locus_tag = case (get_locus_tag f) of
                  Nothing -> "unknown_locus"
                  Just x -> x
                  
  seq_string <- get_translation f
  let seq_label = T.intercalate "|" [gb_version g, locus_tag]
  return (seq_label, seq_string)



get_locus_tag :: Feature -> Maybe T.Text
get_locus_tag Feature{..} = get_feature_qualifier_value "locus_tag" f_qualifiers

--get_protein_id :: Feature -> Maybe T.Text
--get_protein_id Feature{..} = get_feature_qualifier_value "protein_id" f_qualifiers

get_translation :: Feature -> Maybe T.Text
get_translation Feature{..} = get_feature_qualifier_value "translation" f_qualifiers


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


overlaps_region :: FeatureLocation -> (Int, Int) -> Bool
overlaps_region (ContiguousSpan x y) (xx, yy) = are_overlapping (x,y) (xx,yy)
overlaps_region (ImpreciseSpan x y _ _) (xx, yy) = are_overlapping (x,y) (xx,yy)
overlaps_region (Complement x) r = overlaps_region x r
overlaps_region (LocationJoin xs) r = or . map (\x -> overlaps_region x r) $ xs
overlaps_region _ _ = error "ERROR other location constructs not handeled for region overlap check"

within_region :: FeatureLocation -> (Int, Int) -> Bool
within_region (ContiguousSpan x y) (xx, yy) = is_contained (xx,yy) (x,y)
within_region (ImpreciseSpan x y _ _) (xx, yy) = is_contained (xx,yy) (x,y)
within_region (Complement x) r = within_region x r
within_region (LocationJoin xs) r = or . map (\x -> within_region x r) $ xs
within_region _ _ = error "ERROR other location constructs not handeled for within region check"



is_contained :: (Int, Int) -> (Int, Int) -> Bool
is_contained (outer_start, outer_stop) (inner_start, inner_stop) = start_inside && stop_inside
  where
    start_inside = (outer_start <= inner_start) && (inner_start <= outer_stop)
    stop_inside = (outer_start <= inner_stop) && (inner_stop <= outer_stop)

are_overlapping :: (Int, Int) -> (Int, Int) -> Bool
are_overlapping a@(ax,ay) b@(bx,by) = is_contained a (bx,bx) || is_contained a (by,by) || is_contained b (ax,ax) || is_contained b (ay,ay)





--is_feature_inside :: (Int, Int) -> Feature -> Bool
--is_feature_inside region_loci f = is_contained outer (location_to_region . f_location $ f)

is_within_feature :: (Int, Int) -> Feature -> Bool
is_within_feature region_loci f = is_contained (location_to_region . f_location $ f) region_loci


to_record_info :: GenBank -> RecordInfo
to_record_info GenBank{..} = RecordInfo{..}
  where
    ri_accession = gb_version
    ri_source = gb_source
    ri_bp = l_seq_length gb_locus
    ri_circular = l_circular gb_locus
    ri_dblink = case gb_dblink of
                  Nothing -> ""
                  Just x -> x
    ri_definition = gb_definition


record_line :: RecordInfo -> T.Text
record_line RecordInfo{..} = T.intercalate "\t" [ ri_accession, ri_source, T.pack . show $ ri_bp, T.pack . show $ ri_circular, ri_dblink, ri_definition]


al_to_region :: AccessionLocation -> (Int,Int)
al_to_region AccessionLocation{..} = (min al_start al_stop, max al_start al_stop)

filter_extract_origin_seqs :: [AccessionLocation] -> GenBank -> [(T.Text,T.Text)]
filter_extract_origin_seqs xs g@GenBank{..} = seqs
  where
    relevant = filter (\x -> al_accession x == gb_version) xs
    regions = map al_to_region relevant
    cds_features = filter cds_filter gb_features
    overlapping_features = filter (\feature -> or . map (overlaps_region (f_location feature)) $ regions) cds_features
    seqs = map (extract_labeled_sequence g) overlapping_features
  

to_diagram_feature :: Feature -> DiagramFeature
to_diagram_feature f@Feature{..} = DiagramFeature{..}
  where
    
    gene = case (get_feature_qualifier_value "gene" f_qualifiers) of
                Nothing -> ""
                Just x -> x
    product = case (get_feature_qualifier_value "product" f_qualifiers) of
                Nothing -> ""
                Just x -> x
    locus_tag = case (get_feature_qualifier_value "locus_tag" f_qualifiers) of
                Nothing -> ""
                Just x -> x
    df_label = T.intercalate "|" [gene, product, locus_tag]
    df_type = f_feature_key
    (df_start, df_stop) = location_to_region f_location
    df_complement = is_location_complement f_location


-- only checks top level feature location info, if it is nested under join() or order(), it is lost
is_location_complement :: FeatureLocation -> Bool
is_location_complement (Complement _) = True
is_location_complement _ = False


--------------------------------------------------------------------------------


-- the function says "warn" since it doesn't break the program, but could affect the user's results, rely on the user to take appropriate action
warn_non_dna :: GenBank -> IO Bool
warn_non_dna GenBank{..} = case (l_molecule_type gb_locus) of
                             "DNA" -> return False
                             _     -> do
                                        putStrLn [i|WARNING GenBank sequence is not DNA, #{gb_version}, skip|]
                                        return True



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
