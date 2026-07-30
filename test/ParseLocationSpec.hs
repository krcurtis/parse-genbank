--------------------------------------------------------------------------------
--- Test parsing of Genbank locations strings



{-# LANGUAGE OverloadedStrings #-}

module ParseLocationSpec where

import Test.Hspec
import Text.Megaparsec
import Text.Megaparsec.Char

import Test.Hspec.Megaparsec

import qualified Data.Text as T

--------------------------------------------------------------------------------

import ParseFeatureLocation
import FeatureLocation


spec :: Spec
spec = describe "Tests for parsing Genbank location strings" $ do


  it "parse feature location: simple contiguous span" $ do    
    let location_text = "1..34"
        expected = ContiguousSpan 1 34
    parse parse_feature_location "" location_text `shouldParse` expected


  it "parse feature location: half-extended span" $ do
    let location_text = "<1..206"
        expected = ImpreciseSpan 1 206 True False
    parse parse_feature_location "" location_text `shouldParse` expected


  it "parse joined feature location" $ do
    let location_text = "join(1641345..1641428,1641430..1642464)"
        expected = LocationJoin [ContiguousSpan 1641345 1641428, ContiguousSpan 1641430 1642464]
    parse parse_feature_location "" location_text `shouldParse` expected

  it "parse complement" $ do
    let location_text = "complement(join(335780..336814,336816..336899))"
        expected = Complement (LocationJoin [ContiguousSpan 335780 336814, ContiguousSpan 336816 336899])
    parse parse_feature_location "" location_text `shouldParse` expected

  it "parse non-standard single number location" $ do
    let location_text = "complement(join(2298962..2299539,1))"
        expected = Complement (LocationJoin [ContiguousSpan 2298962 2299539, ContiguousSpan 1 1])
    parse parse_feature_location "" location_text `shouldParse` expected

  it "parse non-standard single number location at start" $ do
    let location_text = "join(2291059,1..839)"
        expected = LocationJoin [ContiguousSpan 2291059 2291059, ContiguousSpan 1 839]
    parse parse_feature_location "" location_text `shouldParse` expected

