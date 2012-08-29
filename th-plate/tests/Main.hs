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
import Language.Haskell.TH.Plate


main = defaultMainWithArgs tests ["-a 100", "-o 5"]

tests = []
            

data OtherThing = OtherThing Float

data ThingType = ThingType OtherThing Int

$(do 
     u <- get_universe ''ThingType
     runIO $ do (putStr . show) $ map fst u
     
     u <- runChildTypes =<< reify (mkName "ThingType")
     runIO $ do (putStr . show) u
     
     u <- runChildTypes' =<< reify (mkName "ThingType")
     runIO $ do (putStr . show) u
     
     return [])