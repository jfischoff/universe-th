module Language.Haskell.TH.Universe where
import Language.Haskell.TH
import Control.Monad.State

type Universe = [Dec]

type UniverseState :: ErrorT String (StateT Q Universe)

type Result a = Either String a

create_universe_name :: Name -> UniverseState ()
create_universe_name name = do
    reify_result = reify name 
    case reify_result of
                         ClassI dec _                  -> create_universe_dec dec
                         ClassOpI _ _ dec_name _       -> create_universe_name dec_name
                         TyConI dec                    -> create_universe_dec dec
                         PrimTyConI _ _ _              -> error "Don't know what a PrimTyConI is, but i have a guesss"
                         DataConI _ _ dec_name _       -> create_universe_name dec_name
                         VarI _ _ m_dec _              -> maybe (return $ Right []) create_universe_dec m_dec
                         TyVarI _ _                    -> error "Don't know what a TyVarI is"
        
        
create_universe_dec :: Dec -> UniverseState ()
create_universe_dec dec = do
    --concatMap the Cons and collect the types
    --look them up if haven't looked them up before
