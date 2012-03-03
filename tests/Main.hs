{-# LANGUAGE TupleSections, NoMonomorphismRestriction, TemplateHaskell #-}
module Main where
import Language.Haskell.TH
import Language.Haskell.TH.Universe
import Test.Framework (defaultMain, testGroup, defaultMainWithArgs)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit
import Debug.Trace.Helpers
import Debug.Trace
import Language.Haskell.TH.Instances
import Test.QuickCheck.Checkers
import Data.List
import Data.Generics.Uniplate.Data
import Control.Applicative ((<$>))


main = defaultMainWithArgs tests ["-a 100", "-o 5"]

tests = [
            testGroup "get_type_names" [
                testCase "test_get_type_names_0" test_get_type_names_0,
                testCase "test_get_type_names_1" test_get_type_names_1
            ],
            testGroup "filter_dups'" [
                testCase "test_filter_dups_0" test_filter_dups_0
            ],
            testGroup "collect_dec_types" [
                testCase "test_collect_dec_types_0" test_collect_dec_types_0
            ],
            testGroup "collect_new_dec_names" [
                testCase "test_collect_new_dec_names_0" test_collect_new_dec_names_0
            ]]
            
test_get_type_names_0    = actual @?= [] where
    actual   = get_type_names initial    
    expected = map mkName ["test_0", "test_1"]
    initial  = foldl1' AppT $ map VarT expected
        
test_get_type_names_1    = actual @?= expected where
    actual   = get_type_names initial    
    expected = map mkName ["test_0", "test_1"]
    initial  = foldl1' AppT $ map ConT expected

test_collect_dec_types_0 = actual @?= expected where
        actual   = collect_dec_type_names initial    
        expected = map mkName ["test_0", "test_1"]
        initial  = DataD [] (mkName "Hey") [] [con] []
        con      = NormalC (mkName "Con") [(NotStrict, foldl1' AppT $ map ConT expected)]
        
test_filter_dups_0 = actual @?= expected where
    actual   = filter_dups' (expected ++ [mkName "a"]) initial    
    expected = map mkName ["test_0", "test_1"]
    initial  = [(mkName "a", undefined :: Dec), (mkName "b", undefined :: Dec)]
    
test_collect_new_dec_names_0 = do 
    let expected = map mkName ["test_0", "test_1"]
        initial  = DataD [] (mkName "Hey") [] [con] []
        con      = NormalC (mkName "Con") [(NotStrict, foldl1' AppT $ map ConT expected)]
    actual <- eval_state (collect_new_dec_names initial)
    actual @?= Right expected

data OtherThing = OtherThing Float

data ThingType = ThingType OtherThing Int

$(do 
     u <- get_universe ''ThingType
     runIO $ do (putStr . show) u
     return [])








