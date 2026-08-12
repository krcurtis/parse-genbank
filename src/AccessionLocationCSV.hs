-- Copyright 2026 Fred Hutchinson Cancer Research Center
--------------------------------------------------------------------------------
--- Helper file for loading the CSV containing accessions to sequences
--- and regions within the sequence. No actual sequences.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AccessionLocationCSV where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import Data.ByteString (hGetLine)

import qualified Data.Csv as Cassava
import Data.Csv ((.=), (.:))

import qualified Data.Vector as V


import qualified Data.Map as Map
import qualified Data.Set as Set

--import Data.Bits.Utils (w82c, c2w8)

import System.IO (openFile, IOMode(ReadMode), hClose)


import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import qualified Data.Text as T
import qualified Data.Csv as Cassava

--------------------------------------------------------------------------------

data AccessionLocation =  AccessionLocation
    { al_accession :: T.Text
    , al_start :: Int
    , al_stop :: Int
    } deriving (Show, Eq)


al_stats_columns = [ "accession",
                     "start",
                     "stop"]


instance Cassava.FromNamedRecord AccessionLocation where
    parseNamedRecord m = do
        al_accession  <- fmap decodeUtf8 (m .: "accession")
        al_start       <- (m .: "start")
        al_stop        <- (m .: "stop")
        return $ AccessionLocation{..}




instance Cassava.ToNamedRecord AccessionLocation where
    toNamedRecord AccessionLocation{..} = Cassava.namedRecord
                                      [ "accession"       .= encodeUtf8 al_accession
                                      , "start"           .= al_start
                                      , "stop"            .= al_stop
                                      ]



decode_al_rows :: BL.ByteString -> Either String (Cassava.Header, V.Vector AccessionLocation)
decode_al_rows = Cassava.decodeByName



read_accession_location_csv :: String -> IO [AccessionLocation]
read_accession_location_csv filename = do
  text <- BL.readFile filename
  let Right (_, rows) = decode_al_rows text
  return $ V.toList rows
