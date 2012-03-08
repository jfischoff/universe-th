{-# LANGUAGE TupleSections, NoMonomorphismRestriction, StandaloneDeriving, TemplateHaskell #-}
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
            ],
            testGroup "sub_universe" [
                testCase "test_sub_universe_0" test_sub_universe_0
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

deriving instance Ord Dec
deriving instance Ord Clause
deriving instance Ord Pat
deriving instance Ord Body
deriving instance Ord FunDep
deriving instance Ord Foreign
deriving instance Ord Pragma
deriving instance Ord FamFlavour
deriving instance Ord TyVarBndr
deriving instance Ord Kind
deriving instance Ord Pred
deriving instance Ord Con
deriving instance Ord Strict
deriving instance Ord Safety
deriving instance Ord Callconv
deriving instance Ord Guard
deriving instance Ord Lit
deriving instance Ord Exp
deriving instance Ord InlineSpec
deriving instance Ord Match 
deriving instance Ord Range
deriving instance Ord Stmt

test_sub_universe_0 = do
    let expected          = sort [initial_type, dec_0, dec_1]  
        initial_universe  = [initial_type, dec_0, dec_1, dec_2]
        initial_type      = DataD [] initial_type_name [] [NormalC (mkName "C") 
                                $ map (NotStrict, ) [ConT $ mkName "dec_0", ConT $ mkName "dec_1"]] []
        initial_type_name = mkName "Test_name"
        dec_0             = DataD [] (mkName "dec_0") [] [NormalC (mkName "D") 
                                [(NotStrict, ConT $ mkName "dec_1")]] []
        dec_1             = DataD [] (mkName "dec_1") [] [NormalC (mkName "E") 
                                []] []
        dec_2             = DataD [] (mkName "dec_2") [] [NormalC (mkName "F") 
                                []] []
    let actual            = sub_universe initial_universe initial_type_name
    (sort $ map snd actual) @?= expected 

data OtherThing = OtherThing Float

data ThingType = ThingType OtherThing Int

$(do 
     u <- get_universe ''ThingType
     runIO $ do (putStr . show) u
     return [])
     
     







