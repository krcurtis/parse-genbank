-- Copyright 2026 Fred Hutchinson Cancer Center
--------------------------------------------------------------------------------
-- for feature location per Genbank format description  2004 release 140.0




module FeatureLocation where



data FeatureLocation = CleavageSite { c_site_before :: Int
                                    , c_site_after :: Int
                                    }
                     | ContiguousSpan { cs_start :: Int   -- inclusive coordinates
                                      , cs_stop :: Int
                                      }
                     | ImpreciseSpan { is_start :: Int
                                     , is_stop :: Int
                                     , is_start_imprecise :: Bool
                                     , is_stop_imprecise :: Bool
                                     }
                     | LocationJoin [FeatureLocation]
                     | LocationOrder [FeatureLocation]
                     | Complement FeatureLocation
                     deriving (Show, Eq)


