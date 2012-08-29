{-# LANGUAGE TupleSections, RankNTypes, FlexibleInstances, DeriveFunctor #-}
module Language.Haskell.TH.Plate.Folds where
import Language.Haskell.TH.Plate.Types
import Data.Generics.Multiplate
import Language.Haskell.TH.Syntax hiding (lift)
import Control.Applicative
import Data.Traversable hiding (sequence)
import Data.Functor.Constant
import Data.Functor.Identity
import Data.List
import Data.Monoid
import Control.Monad
import Data.Generics.Uniplate.Data
import Control.Monad.Reader

newtype MQ a = MQ { unMQ :: a}
    deriving (Functor)
    
instance (Monad m, Monoid a) => Monoid (MQ (m a)) where
    mempty                = MQ $ return mempty
    mappend (MQ x) (MQ y) = MQ $ liftM2 mappend x y
  
type Context = ReaderT [Name] Q

runChildTypes :: Info -> Q [Type]
runChildTypes i = runReaderT (childTypes i) [] 

childTypes :: Info -> Context [Type]
childTypes = 
     fmap nub . unMQ . foldFor infoPL (preorderFold purePlate { typPL = go } )  
        where
          go x@(ConT _ ) = Constant . MQ $ recurseOnce childTypes x
          go (VarT _ )   = Constant . MQ $ return [] 
          go (ArrowT )   = Constant . MQ $ return []
          go (ListT  )   = Constant . MQ $ return []
          go x           = Constant . MQ $ return [x] 

--Using Biplate 
runChildTypes' :: Info -> Q [Type]
runChildTypes' i = runReaderT (childTypes' i) []
         
childTypes' :: Info -> Context [Type] 
childTypes' i = 
    fmap (nub . concat) $ sequence $ [ go t | t <- universeBi i ] 
        where
          go x@(ConT _ ) = recurseOnce childTypes' x
          go (VarT _ )   = return [] 
          go (ArrowT )   = return []
          go (ListT  )   = return []
          go x           = return [x] 
          
 
recurseOnce :: (Info -> Context [Type]) -> Type -> Context [Type]
recurseOnce f x@(ConT n ) = do 
    has <- asks (elem n)
    if has 
        then return []
        else local (n:) $ fmap (x:) $ f =<< (lift $ reify n)
