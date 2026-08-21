-- Copyright 2026 Fred Hutchinson Cancer Center
--------------------------------------------------------------------------------
-- Simplified structure for making diagrams of genes and various regions



{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}


module GenomeDiagramRegion where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as BL


import Data.Aeson
import Data.Aeson.Types
-- import GHC.Generics


--------------------------------------------------------------------------------


data DiagramRegion = DiagramRegion { dr_genome :: T.Text -- source genome
                                   , dr_accession :: T.Text -- version field from Genbank
                                   , dr_locus_tag :: T.Text  -- region located around this locus tag
                                   , dr_start :: Int
                                   , dr_stop :: Int
                                   , dr_features :: [DiagramFeature]
                                   }
     deriving (Show)

data DiagramFeature = DiagramFeature { df_label :: T.Text -- probably product field if CDS
                                     , df_type :: T.Text -- like CDS, forward_primer, probe, reverse_primer
                                     , df_start :: Int
                                     , df_stop :: Int
                                     , df_complement :: Bool
                                     }
     deriving(Show)



instance ToJSON DiagramFeature where
  toJSON DiagramFeature{..} = object [ "label" .= df_label
                                     , "type" .= df_type
                                     , "start" .= df_start
                                     , "stop" .= df_stop
                                     , "complement" .= df_complement ]

instance ToJSON DiagramRegion where
  toJSON DiagramRegion{..} = object [ "genome" .= dr_genome
                                    , "accession" .= dr_accession
                                    , "locus_tag" .= dr_locus_tag
                                    , "start" .= dr_start
                                    , "stop" .= dr_stop
                                    , "features" .= dr_features ]





write_diagram_region_file :: String -> DiagramRegion -> IO ()
write_diagram_region_file filename x = do
  encodeFile filename x
