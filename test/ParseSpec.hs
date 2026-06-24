--------------------------------------------------------------------------------
--- Test parsing of GenBank files



{-# LANGUAGE OverloadedStrings #-}

module ParseSpec where

import Test.Hspec
import Text.Megaparsec
import Text.Megaparsec.Char

import Test.Hspec.Megaparsec

import qualified Data.Text as T

--------------------------------------------------------------------------------

import ParseGenBank
import FeatureLocation

test_file = "sequence.gb"


feature1 :: T.Text
feature1 = T.pack . unlines $ [ "     source          1..34"
                              , "                     /organism=\"synthetic construct\""
                              , "                     /mol_type=\"other RNA\""
                              , "                     /db_xref=\"taxon:32630\""
                              , "ORIGIN      "]



feature1_out :: Feature
feature1_out = Feature { f_feature_key = "source"
                       , f_location = ContiguousSpan 1 34
                       , f_qualifiers = [ Qualifier {q_name = "organism", q_value = "synthetic construct"}
                                        , Qualifier {q_name = "mol_type", q_value = "other RNA"}
                                        , Qualifier {q_name = "db_xref", q_value = "taxon:32630"}]
                       }


genbank_tiny :: T.Text
genbank_tiny = T.pack . unlines $ [ "LOCUS       3K1V_A                    34 bp    RNA     linear   SYN 21-FEB-2024"
                                  , "DEFINITION  Chain A, PreQ1 riboswitch."
                                  , "ACCESSION   3K1V_A"
                                  , "VERSION     3K1V_A"
                                  , "KEYWORDS    ."
                                  , "SOURCE      synthetic construct"
                                  , "  ORGANISM  synthetic construct"
                                  , "            other sequences; artificial sequences."
                                  , "REFERENCE   1  (bases 1 to 34)"
                                  , "  AUTHORS   Klein,D.J., Edwards,T.E. and Ferr&#xe9;-D'Amar&#xe9;,A.R."
                                  , "  TITLE     Cocrystal structure of a class I preQ1 riboswitch reveals a"
                                  , "            pseudoknot recognizing an essential hypermodified nucleobase"
                                  , "  JOURNAL   Nat Struct Mol Biol 16 (3), 343-344 (2009)"
                                  , "   PUBMED   19234468"
                                  , "REFERENCE   2  (bases 1 to 34)"
                                  , "  AUTHORS   Klein,D.J., Edwards,T.E. and Ferre-D'Amare,A.R."
                                  , "  TITLE     Direct Submission"
                                  , "  JOURNAL   Submitted (28-SEP-2009)"
                                  , "COMMENT     Cocrystal structure of a mutant class-I preQ1 riboswitch."
                                  , "FEATURES             Location/Qualifiers"
                                  , "     source          1..34"
                                  , "                     /organism=\"synthetic construct\""
                                  , "                     /mol_type=\"other RNA\""
                                  , "                     /db_xref=\"taxon:32630\""
                                  , "ORIGIN      "
                                  , "        1 agaggttcta gcacatccct ctataaaaaa ctaa"
                                  , "//"
                                  , ""
                                  , ""]


expected_ref1 = Reference { r_ref_number = 1
                          , r_base_range = (1, 34)
                          , r_authors = "Klein,D.J., Edwards,T.E. and Ferr&#xe9;-D'Amar&#xe9;,A.R."
                          , r_consortium = Nothing                          
                          , r_title = "Cocrystal structure of a class I preQ1 riboswitch reveals a pseudoknot recognizing an essential hypermodified nucleobase"
                          , r_journal = "Nat Struct Mol Biol 16 (3), 343-344 (2009)"
                          , r_medline = Nothing
                          , r_pmid = Just "19234468"
                          , r_remark = Nothing}

expected_ref2 = Reference { r_ref_number = 2
                          , r_base_range = (1, 34)
                          , r_authors = "Klein,D.J., Edwards,T.E. and Ferre-D'Amare,A.R."
                          , r_consortium = Nothing
                          , r_title = "Direct Submission"
                          , r_journal = "Submitted (28-SEP-2009)"
                          , r_medline = Nothing
                          , r_pmid = Nothing
                          , r_remark = Nothing}





expected_tiny :: GenBank
expected_tiny = GenBank { gb_locus = Locus "3K1V_A" 34 UnknownStranded "RNA" False (Just "SYN") "21-FEB-2024"
                       , gb_definition = "Chain A, PreQ1 riboswitch."
                       , gb_accession = "3K1V_A"
                       , gb_version = "3K1V_A"
                       , gb_keywords = "."
                       , gb_segment = Nothing
                       , gb_source = "synthetic construct"
                       , gb_organism = "synthetic construct other sequences; artificial sequences."
                       , gb_references = [expected_ref1, expected_ref2]
                       , gb_comment = Just "Cocrystal structure of a mutant class-I preQ1 riboswitch."
                       , gb_features = [feature1_out]
                       , gb_origin_sequence = "agaggttctagcacatccctctataaaaaactaa"
                       }


spec :: Spec
spec = describe "Tests for parsing GenBank format" $ do

  it "parse only spaces" $ do
    let x = "  \n\n\n\n"
        expected = "  "
    parse spaces "" x `shouldParse` expected

    
  it "parse a single qualifier" $ do
    let qualifier_line = T.pack . unlines $ [ "                     /codon_start=3"
                                            , ""
                                            ]
        expected = Qualifier { q_name = "codon_start", q_value = "3" }
    parse (parse_qualifier 21) "" qualifier_line `shouldParse` expected


  it "parse a single feature" $ do
    parse parse_feature "" feature1 `shouldParse` feature1_out



  it "parse two features" $ do

    let feature_lines = T.pack . unlines $ [ "     source          1..5028"
                                           , "                     /organism=\"Saccharomyces cerevisiae\""
                                           , "                     /mol_type=\"genomic DNA\""
                                           , "                     /db_xref=\"taxon:4932\""
                                           , "                     /chromosome=\"IX\""
                                           , "     mRNA            <1..>206"
                                           , "                     /product=\"TCP1-beta\""
                                           , "ORIGIN      "]


        feature1_out = Feature { f_feature_key = "source"
                               , f_location = ContiguousSpan 1 5028
                               , f_qualifiers = [ Qualifier {q_name = "organism", q_value = "Saccharomyces cerevisiae"}
                                                , Qualifier {q_name = "mol_type", q_value = "genomic DNA"}
                                                , Qualifier {q_name = "db_xref", q_value = "taxon:4932"}
                                                , Qualifier {q_name = "chromosome", q_value = "IX"}
                                                ] }
        feature2_out = Feature { f_feature_key = "mRNA"
                               , f_location = ImpreciseSpan 1 206 True True
                               , f_qualifiers = [ Qualifier {q_name = "product", q_value = "TCP1-beta"}
                                                ] }

        expected = [feature1_out, feature2_out]

    parse (some parse_feature) "" feature_lines `shouldParse` expected

  it "parse CDS feature" $ do

    let feature_lines = T.pack . unlines $ [ "     CDS             <1..206"
                                           , "                     /codon_start=3"
                                           , "                     /product=\"TCP1-beta\""
                                           , "                     /protein_id=\"AAA98665.1\""
                                           , "                     /translation=\"SSIYNGISTSGLDLNNGTIADMRQLGIVESYKLKRAVVSSASEA"
                                           , "                     AEVLLRVDNIIRARPRTANRQHM\""
                                           , "ORIGIN      "]


        feature1_out = Feature { f_feature_key = "CDS"
                               , f_location = ImpreciseSpan 1 206 True False
                               , f_qualifiers = [ Qualifier {q_name = "codon_start", q_value = "3"}
                                                , Qualifier {q_name = "product", q_value = "TCP1-beta"}
                                                , Qualifier {q_name = "protein_id", q_value = "AAA98665.1"}
                                                , Qualifier {q_name = "translation", q_value = "SSIYNGISTSGLDLNNGTIADMRQLGIVESYKLKRAVVSSASEAAEVLLRVDNIIRARPRTANRQHM"}
                                              ]}
        expected = [feature1_out]

    parse (some parse_feature) "" feature_lines `shouldParse` expected


  it "parse feature with location-like qualifier" $ do
    let feature_lines = T.pack . unlines $ [ "     tRNA            complement(1032828..1032913)"
                                           , "                     /locus_tag=\"J5W47_04370\""
                                           , "                     /product=\"tRNA-Tyr\""
                                           , "                     /inference=\"COORDINATES: profile:tRNAscan-SE:2.0.7\""
                                           , "                     /note=\"Derived by automated computational analysis using"
                                           , "                     gene prediction method: tRNAscan-SE.\""
                                           , "                     /anticodon=(pos:complement(1032877..1032879),aa:Tyr,"
                                           , "                     seq:gta)"
                                           , "ORIGIN      "]
        feature1_out = Feature { f_feature_key = "tRNA"
                               , f_location = Complement (ContiguousSpan 1032828 1032913)
                               , f_qualifiers = [ Qualifier {q_name = "locus_tag", q_value = "J5W47_04370"}
                                                , Qualifier {q_name = "product", q_value = "tRNA-Tyr"}
                                                , Qualifier {q_name = "inference", q_value = "COORDINATES: profile:tRNAscan-SE:2.0.7"}
                                                , Qualifier {q_name = "note", q_value = "Derived by automated computational analysis using gene prediction method: tRNAscan-SE."}
                                                , Qualifier {q_name = "anticodon", q_value = "(pos:complement(1032877..1032879),aa:Tyr, seq:gta)"}
                                              ]}
        expected = [feature1_out]
    parse (some parse_feature) "" feature_lines `shouldParse` expected


  it "parse origin sequence [single line]" $ do
    let origin_sequence_text = T.pack . unlines $ [ "ORIGIN      "
                                                  , "        1 agaggttcta gcacatccct ctataaaaaa ctaa"
                                                  ]
        expected = "agaggttctagcacatccctctataaaaaactaa"

    parse parse_origin "" origin_sequence_text `shouldParse` expected


  it "parse origin sequence [multi-line]" $ do
    let origin_sequence_text = T.pack . unlines $ [ "ORIGIN      "
                                                  , "        1 tcgcgcgttt cggtgatgac ggtgaaaacc tctgacacat gcagctcccg gagacggtca"
                                                  , "       61 cagcttgtct gtaagcggat gccgggagca gacaagcccg tcagggcgcg tcagcgggtg"
                                                  , "      121 ttggcgggtg tcggggctgg cttaactatg cggcatcaga gcagattgta ctgagagtgc" ]
        expected = T.concat ["tcgcgcgtttcggtgatgacggtgaaaacctctgacacatgcagctcccggagacggtca"
                            , "cagcttgtctgtaagcggatgccgggagcagacaagcccgtcagggcgcgtcagcgggtg"
                            , "ttggcgggtgtcggggctggcttaactatgcggcatcagagcagattgtactgagagtgc"]

    parse parse_origin "" origin_sequence_text `shouldParse` expected


  it "flatten origin sequence text list into non-spaced letters" $ do
    let text_input = ["tcgcgcgttt cggtgatgac ggtgaaaacc tctgacacat gcagctcccg gagacggtca"
                     , "cagcttgtct gtaagcggat gccgggagca gacaagcccg tcagggcgcg tcagcgggtg"
                     ,  "ttggcgggtg tcggggctgg cttaactatg cggcatcaga gcagattgta ctgagagtgc"]
        expected = "tcgcgcgtttcggtgatgacggtgaaaacctctgacacatgcagctcccggagacggtcacagcttgtctgtaagcggatgccgggagcagacaagcccgtcagggcgcgtcagcgggtgttggcgggtgtcggggctggcttaactatgcggcatcagagcagattgtactgagagtgc"
    flatten_sequence_text text_input `shouldBe` expected


  it "parse entry field" $ do
    let field_text = T.pack . unlines $ [ "DEFINITION  Cloning vector pUC19c, complete sequence."
                                        , "ACCESSION   L09137 X02514"]
        expected = "Cloning vector pUC19c, complete sequence."
    parse (parse_entry "DEFINITION") "" field_text `shouldParse` expected


  it "parse locus line" $ do
    let input_text = T.pack . unlines $ [ "LOCUS       3K1V_A                    34 bp    RNA     linear   SYN 21-FEB-2024"
                                        , "DEFINITION  Chain A, PreQ1 riboswitch."
                                        ]
        expected = Locus "3K1V_A" 34 UnknownStranded "RNA" False (Just "SYN") "21-FEB-2024"
    parse parse_locus "" input_text `shouldParse` expected



  it "parse reference" $ do
    let ref_text = T.pack . unlines $ [ "REFERENCE   1  (bases 1 to 2686)"
                                      , "  AUTHORS   Yanisch-Perron,C., Vieira,J. and Messing,J."
                                      , "  TITLE     Improved M13 phage cloning vectors and host strains: nucleotide"
                                      , "            sequences of the M13mp18 and pUC19 vectors"
                                      , "  JOURNAL   Gene 33 (1), 103-119 (1985)"
                                      , "   PUBMED   2985470"
                                      , "COMMENT     On Apr 11, 2002 this sequence version replaced L09137.1." ]
        expected = Reference { r_ref_number = 1
                             , r_base_range = (1,2686)
                             , r_authors = "Yanisch-Perron,C., Vieira,J. and Messing,J."
                             , r_consortium = Nothing
                             , r_title = "Improved M13 phage cloning vectors and host strains: nucleotide sequences of the M13mp18 and pUC19 vectors"
                             , r_journal = "Gene 33 (1), 103-119 (1985)"
                             , r_medline =  Nothing
                             , r_pmid = Just "2985470"
                             , r_remark = Nothing }
    parse parse_reference "" ref_text `shouldParse` expected


  it "parse journal" $ do
    let input_text = T.pack . unlines $ [ "  JOURNAL   Submitted (27-APR-1993) Department of Biochemistry, University of"
                                        , "            Minnesota, St. Paul, MN 55108, USA"
                                        , "REFERENCE   5  (bases 1 to 2686)"]
        expected = "Submitted (27-APR-1993) Department of Biochemistry, University of Minnesota, St. Paul, MN 55108, USA"
    parse (parse_entry "JOURNAL") "" input_text `shouldParse` expected


  it "parse empty GenBank lines, check failure" $ do
    let empty = ""
    parse parse_genbank "" `shouldFailOn` empty

  it "parse incomplete GenBank lines [missing definition onward], check failure" $ do
    let incomplete = T.pack . unlines $ [ "LOCUS       3K1V_A                    34 bp    RNA     linear   SYN 21-FEB-2024"
                                        ]
    parse parse_genbank "" `shouldFailOn` incomplete


  it "parse incomplete GenBank lines [missing accession onward], check failure" $ do
    let incomplete = T.pack . unlines $ [ "LOCUS       3K1V_A                    34 bp    RNA     linear   SYN 21-FEB-2024"
                                        , "DEFINITION  Chain A, PreQ1 riboswitch."
                                        ]
    parse parse_genbank "" `shouldFailOn` incomplete


  it "parse incomplete GenBank lines [missing keywords onward], check failure" $ do
    let incomplete = T.pack . unlines $ [ "LOCUS       3K1V_A                    34 bp    RNA     linear   SYN 21-FEB-2024"
                                        , "DEFINITION  Chain A, PreQ1 riboswitch."
                                        , "ACCESSION   3K1V_A"
                                        , "VERSION     3K1V_A"
                                        ]
    parse parse_genbank "" `shouldFailOn` incomplete

  it "parse incomplete GenBank lines [missing reference onward], check failure" $ do
    let incomplete = T.pack . unlines $ [ "LOCUS       3K1V_A                    34 bp    RNA     linear   SYN 21-FEB-2024"
                                        , "DEFINITION  Chain A, PreQ1 riboswitch."
                                        , "ACCESSION   3K1V_A"
                                        , "VERSION     3K1V_A"
                                        , "KEYWORDS    ."
                                        , "SOURCE      synthetic construct"
                                        , "  ORGANISM  synthetic construct"
                                        , "            other sequences; artificial sequences."
                                        ]
    parse parse_genbank "" `shouldFailOn` incomplete

  it "parse incomplete GenBank lines [missing features onward], check failure" $ do
    let incomplete = T.pack . unlines $ [ "LOCUS       3K1V_A                    34 bp    RNA     linear   SYN 21-FEB-2024"
                                        , "DEFINITION  Chain A, PreQ1 riboswitch."
                                        , "ACCESSION   3K1V_A"
                                        , "VERSION     3K1V_A"
                                        , "KEYWORDS    ."
                                        , "SOURCE      synthetic construct"
                                        , "  ORGANISM  synthetic construct"
                                        , "            other sequences; artificial sequences."
                                        , "REFERENCE   1  (bases 1 to 34)"
                                        , "  AUTHORS   Klein,D.J., Edwards,T.E. and Ferr&#xe9;-D'Amar&#xe9;,A.R."
                                        , "  TITLE     Cocrystal structure of a class I preQ1 riboswitch reveals a"
                                        , "            pseudoknot recognizing an essential hypermodified nucleobase"
                                        , "  JOURNAL   Nat Struct Mol Biol 16 (3), 343-344 (2009)"
                                        , "   PUBMED   19234468"
                                        ]
    parse parse_genbank "" `shouldFailOn` incomplete

  it "parse incomplete GenBank lines [missing end], check failure" $ do
    let incomplete = T.pack . unlines $ [ "LOCUS       3K1V_A                    34 bp    RNA     linear   SYN 21-FEB-2024"
                                        , "DEFINITION  Chain A, PreQ1 riboswitch."
                                        , "ACCESSION   3K1V_A"
                                        , "VERSION     3K1V_A"
                                        , "KEYWORDS    ."
                                        , "SOURCE      synthetic construct"
                                        , "  ORGANISM  synthetic construct"
                                        , "            other sequences; artificial sequences."
                                        , "REFERENCE   1  (bases 1 to 34)"
                                        , "  AUTHORS   Klein,D.J., Edwards,T.E. and Ferr&#xe9;-D'Amar&#xe9;,A.R."
                                        , "  TITLE     Cocrystal structure of a class I preQ1 riboswitch reveals a"
                                        , "            pseudoknot recognizing an essential hypermodified nucleobase"
                                        , "  JOURNAL   Nat Struct Mol Biol 16 (3), 343-344 (2009)"
                                        , "   PUBMED   19234468"
                                        , "REFERENCE   2  (bases 1 to 34)"
                                        , "  AUTHORS   Klein,D.J., Edwards,T.E. and Ferre-D'Amare,A.R."
                                        , "  TITLE     Direct Submission"
                                        , "  JOURNAL   Submitted (28-SEP-2009)"
                                        , "COMMENT     Cocrystal structure of a mutant class-I preQ1 riboswitch."
                                        , "FEATURES             Location/Qualifiers"
                                        , "     source          1..34"
                                        , "                     /organism=\"synthetic construct\""
                                        , "                     /mol_type=\"other RNA\""
                                        , "                     /db_xref=\"taxon:32630\""
                                        , "ORIGIN      "
                                        , "        1 agaggttcta gcacatccct ctataaaaaa ctaa"
                                        ]
    parse parse_genbank "" `shouldFailOn` incomplete



  it "parse GenBank lines [complete, tiny]" $ do
    parse parse_genbank "" genbank_tiny `shouldParse` expected_tiny




  it "parse longer comment" $ do
    let comment = T.pack . unlines $ [ "COMMENT     The annotation was added by the NCBI Prokaryotic Genome Annotation"
                                     , "            Pipeline (PGAP). Information about PGAP can be found here:"
                                     , "            https://www.ncbi.nlm.nih.gov/genome/annotation_prok/"
                                     , "            "
                                     , "            ##Genome-Assembly-Data-START##"
                                     , "            Assembly Method        :: Flye v. 2.6"
                                     , "FEATURES             Location/Qualifiers"
                                     ]
        expected = T.intercalate " " $ [ "The annotation was added by the NCBI Prokaryotic Genome Annotation"
                                       , "Pipeline (PGAP). Information about PGAP can be found here:"
                                       , "https://www.ncbi.nlm.nih.gov/genome/annotation_prok/"
                                       , ""
                                       , "##Genome-Assembly-Data-START##"
                                       , "Assembly Method        :: Flye v. 2.6"
                                       ]

    parse (optional (try (parse_entry "COMMENT"))) "" comment `shouldParse` Just expected



  it "parse prokka locus info" $ do
    let prokka_text = T.pack . unlines $ [ "LOCUS       CP110665.1           2675865 bp    DNA     linear       26-MAY-2026"
                                         , "DEFINITION  Genus species strain strain."
                                         ]
        expected = Locus "CP110665.1" 2675865  UnknownStranded "DNA"  False Nothing "26-MAY-2026"
    parse parse_locus "" prokka_text `shouldParse` expected
        
                   

  it "parse prokka empty accession" $ do
    let prokka_text = T.pack . unlines $ [ "ACCESSION   "
                                         , "VERSION"
                                         , "KEYWORDS    ."
                                         ]
        expected = T.pack ""
    parse (parse_empty_entry "ACCESSION") "" prokka_text `shouldParse` expected

  it "parse prokka empty version" $ do
    let prokka_text = T.pack . unlines $ [ "VERSION"
                                         , "KEYWORDS    ."
                                         ]
        expected = T.pack ""
    parse (parse_empty_entry "VERSION") "" prokka_text `shouldParse` expected




  it "parse prokka header" $ do
    let prokka_text = T.pack . unlines $ [ "LOCUS       CP110665.1           2675865 bp    DNA     linear       26-MAY-2026"
                                         , "DEFINITION  Genus species strain strain."
                                         , "ACCESSION   "
                                         , "VERSION"
                                         , "KEYWORDS    ."
                                         , "SOURCE      Genus species"
                                         , "  ORGANISM  Genus species"
                                         , "            Unclassified."
                                         , "COMMENT     Annotated using prokka 1.14.5 from"
                                         , "            https://github.com/tseemann/prokka."
                                         , "FEATURES             Location/Qualifiers"
                                        , "     source          1..34"
                                        , "                     /organism=\"synthetic construct\""
                                        , "                     /mol_type=\"other RNA\""
                                        , "                     /db_xref=\"taxon:32630\""
                                        , "ORIGIN      "
                                        , "        1 agaggttcta gcacatccct ctataaaaaa ctaa"
                                        , "//"
                                        , ""
                                        , ""
                                        ]
        expected = GenBank { gb_locus = Locus "CP110665.1" 2675865 UnknownStranded "DNA" False Nothing "26-MAY-2026"
                           , gb_definition = "Genus species strain strain."
                           , gb_accession = ""
                           , gb_version = ""
                           , gb_keywords = "."
                           , gb_segment = Nothing
                           , gb_source = "Genus species"
                           , gb_organism = "Genus species Unclassified."
                           , gb_references = []
                           , gb_comment = Just "Annotated using prokka 1.14.5 from https://github.com/tseemann/prokka."
                           , gb_features = [feature1_out]
                           , gb_origin_sequence = "agaggttctagcacatccctctataaaaaactaa"
                           }
    parse parse_genbank "" prokka_text `shouldParse` expected



  it "parse empty feature qualifier" $ do 
    let prokka_text = T.pack . unlines $ [ "     gene            complement(2818816..2819943)"
                                         , "                     /locus_tag=\"SI90_12270\""
                                         , "                     /note=\"hypothetical protein; disrupted; Derived by"
                                         , "                     automated computational analysis using gene prediction"
                                         , "                     method: Protein Homology.\""
                                         , "                     /pseudo"
                                         , "ORIGIN      "
                                         ]
        expected = Feature { f_feature_key = "gene"
                           , f_location = Complement (ContiguousSpan 2818816 2819943)
                           , f_qualifiers = [ Qualifier {q_name = "locus_tag", q_value = "SI90_12270"}
                                            , Qualifier {q_name = "note", q_value = "hypothetical protein; disrupted; Derived by automated computational analysis using gene prediction method: Protein Homology."}
                                            , Qualifier {q_name = "pseudo", q_value = ""}] }
    parse parse_feature "" prokka_text `shouldParse` expected




