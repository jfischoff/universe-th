{-# LANGUAGE TupleSections #-}
module Language.Haskell.TH.Universe (getUniverse) where
import Language.Haskell.TH
import Data.List (nub)
import Control.Monad
import Data.Generics.Uniplate.Data
import Control.Monad.Reader

-- | Collect all types that are named type and its decendents
getUniverse :: Name -> Q [Type]
getUniverse name = do
	info <- reify name
	runChildTypes' info

type Context = ReaderT [Name] Q
	
runChildTypes' :: Info -> Q [Type]
runChildTypes' i = runReaderT (childTypes' i) []

childTypes' :: Info -> Context [Type] 
childTypes' i = 
    fmap (nub . concat) . sequence $ [ go t | t <- universeBi i ] 
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
        else local (n:) . fmap (x:) $ f =<< (lift . reify $ n)


collectConstructors :: Name -> Q [Con]
collectConstructors n = fmap universeBi $ reify n















