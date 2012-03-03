{-# LANGUAGE GeneralizedNewtypeDeriving, NoMonomorphismRestriction,
    FlexibleInstances, FlexibleContexts #-}
module Language.Haskell.TH.Universe (
    get_universe, 
    -- ** Utils ... Not sure how to hide these.
    get_type_names,
    filter_dups',
    collect_new_dec_names,
    collect_dec_type_names,
    eval_state) where
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
import Control.Monad

type Universe = [(Name, Dec)]

type ErrorStateType m e s a = ErrorT e (StateT s m) a

newtype ErrorStateT e s m a = ErrorStateT { runErrorStateT :: ErrorStateType m e s a }
    deriving (Monad, MonadState s, MonadError e, Functor, MonadPlus)
    
instance MonadTrans (ErrorStateT String Universe) where
    lift = ErrorStateT . lift . lift

type UniverseState a = ErrorStateT String Universe Q a
 
type Result a = Either String a


-- | Collect all the ancestor Dec's for whatever is passed in by name. 
--   For instance if we have
-- 
-- > data Otherthing = Otherthing Float
--  
-- > data Thing = Thing OtherThing Int
-- 
--   then 
-- 
-- > get_universe ''Thing
--
-- would return the Dec's for Thing, OtherThing, Int and Float
get_universe :: Name -> Q (Universe)
get_universe = exec_state . create_universe_name


create_universe_name :: Name -> UniverseState ()
create_universe_name name = do
    reify_result <- lift $ reify name 
    case reify_result of
                         ClassI dec _            -> create_universe_dec dec
                         ClassOpI _ _ dec_name _ -> create_universe_name dec_name
                         TyConI dec              -> create_universe_dec dec
                         (PrimTyConI _ _ _)      -> return ()
                         DataConI _ _ dec_name _ -> create_universe_name dec_name
                         VarI _ _ m_dec _        -> maybe (return ()) create_universe_dec m_dec
                         TyVarI _ _              -> error "Don't know what a TyVarI is"
        
-- | Collect all the ancestor Dec's for the given Dec
create_universe_dec :: Dec -> UniverseState ()
create_universe_dec dec = mapM_ create_universe_name =<< collect_new_dec_names dec

collect_new_dec_names :: (Monad m,
    MonadState Universe m, 
    MonadError String   m) => Dec -> m [Name]
collect_new_dec_names dec = do
    dec_name <- get_dec_name dec 
    modify ((dec_name, dec):)
    let type_names = collect_dec_type_names dec
    --look them up if haven't looked them up before
    filter_dups type_names
    

collect_dec_type_names :: Dec -> [Name]
collect_dec_type_names = concatMap get_type_names . nub . concatMap get_con_types . get_cons

filter_dups' :: Eq a => [a] -> [(a, b)] -> [a]
filter_dups' names uni = names \\ (fst $ unzip uni)
    
filter_dups :: (Eq a, MonadState [(a, b)] m) =>[a] -> m [a]
filter_dups names = gets (filter_dups' names) 
    
--------------------------------------------------------------------------------
--utility functions without a home
    
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

from_right :: Either a b -> b
from_right (Right x) = x
from_right _ = error "from_right"

get_type_name' :: Type -> Name
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

get_dec_name :: (MonadError String m, Monad m) => Dec -> m Name
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

exec_state :: Monad m => ErrorStateT e [a1] m a -> m [a1]
exec_state x = execStateT (runErrorT (runErrorStateT x)) []

eval_state :: Monad m => ErrorStateT e [a1] m a -> m (Either e a)
eval_state x = evalStateT (runErrorT (runErrorStateT x)) []















