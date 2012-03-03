{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Language.Haskell.TH.Universe where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Trans
import Data.Generics.Uniplate.Data
import Data.List
import Data.Tuple.Select
import Control.Applicative
import Data.Composition

type Universe = [(Name, Dec)]

type ErrorStateType m e s a = ErrorT e (StateT s m) a

newtype ErrorStateT e s m a = ErrorStateT { runErrorStateT :: ErrorStateType m e s a }
    deriving (Monad, MonadState s, MonadError e, Functor)
    
instance MonadTrans (ErrorStateT String Universe) where
    lift = ErrorStateT . lift . lift

type UniverseState a = ErrorStateT String Universe Q a
 
type Result a = Either String a

create_universe_name :: Name -> UniverseState ()
create_universe_name name = do
    reify_result <- lift $ reify name 
    case reify_result of
                         ClassI dec _                  -> create_universe_dec dec
                         ClassOpI _ _ dec_name _       -> create_universe_name dec_name
                         TyConI dec                    -> create_universe_dec dec
                         PrimTyConI _ _ _              -> error "Don't know what a PrimTyConI is, but i have a guesss"
                         DataConI _ _ dec_name _       -> create_universe_name dec_name
                         VarI _ _ m_dec _              -> maybe (return ()) create_universe_dec m_dec
                         TyVarI _ _                    -> error "Don't know what a TyVarI is"
        
        
create_universe_dec :: Dec -> UniverseState ()
create_universe_dec dec = do
    let dec_name_result = get_dec_name dec 
    dec_name <- case dec_name_result of
                    (Right x) -> return x
                    (Left x)  -> throwError x
    modify ((dec_name, dec):)
    --concatMap the Cons and collect the types
    let type_names = concatMap get_type_names $ nub $ concatMap get_con_types $ get_cons dec
    --look them up if haven't looked them up before
    new_names <- filter_dups type_names
    mapM_ create_universe_name new_names
    
filter_dups :: [Name] -> UniverseState [Name]
filter_dups names = gets (\x -> filter (not . (flip elem $ fst $ unzip x)) names) 
    
get_cons :: Dec -> [Con]
get_cons (NewtypeD _ _ _ con _)      = [con]
get_cons (DataD _ _ _ cons _)        = cons
get_cons (DataInstD _ _ _ cons _)    = cons
get_cons (NewtypeInstD _ _ _ con _)  = [con]
get_cons _                           = []

get_con_types :: Con -> [Type]
get_con_types (NormalC _ st)    = map snd st
get_con_types (RecC _ st)       = map sel3 st
get_con_types (InfixC x _ y)    = map snd [x, y]
get_con_types (ForallC _ _ con) = get_con_types con

get_type_names :: Type -> [Name]
get_type_names = map (from_right . get_type_name) . get_constr_types

from_right (Right x) = x

get_type_name' = from_right . get_type_name

get_type_name :: Type -> Result Name
get_type_name (ForallT _ _ typ) = get_type_name typ
get_type_name (VarT n)          = Right n
get_type_name (ConT n)          = Right n
get_type_name x                 = Left ("No name for " ++ show x)

get_constr_types :: Type -> [Type]
get_constr_types = filter is_cont . universe

is_cont :: Type -> Bool
is_cont (ConT _) = True
is_cont _        = False

get_dec_name :: Dec -> Result Name
get_dec_name (FunD name _)            = return name
get_dec_name (ValD _ _ _)             = throwError "InstanceD does not have a name"
get_dec_name (DataD _ name _ _ _)     = return name
get_dec_name (NewtypeD _ name _ _ _)  = return name
get_dec_name (TySynD name _ _)        = return name
get_dec_name (ClassD _ name _ _ _ )   = return name
get_dec_name (InstanceD _ _ _)        = throwError "InstanceD does not have a name"
get_dec_name (SigD name _)            = return name
get_dec_name (ForeignD _)             = throwError "ForeignD does not have a name"
get_dec_name (PragmaD  _)             = throwError "PragmaD does not have a name"
get_dec_name (FamilyD _ name _ _)     = return name
get_dec_name (DataInstD _ name _ _ _) = return name
















