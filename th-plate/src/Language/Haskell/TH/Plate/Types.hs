{-# LANGUAGE TupleSections, RankNTypes #-}
module Language.Haskell.TH.Plate.Types where

import Data.Generics.Multiplate
import Language.Haskell.TH.Syntax
import Control.Applicative
import Data.Traversable

type GuardedExp = (Guard, Exp)

data THPlate f = THPlate {
        infoPL                 :: Info            -> f Info,               
        decPL                  :: Dec             -> f Dec,                
        clausePL               :: Clause          -> f Clause,             
        conPL                  :: Con             -> f Con,                
        strictTypePL           :: StrictType      -> f StrictType,         
        varStrictTypePL        :: VarStrictType   -> f VarStrictType,      
        patPL                  :: Pat             -> f Pat,                
        fieldPatPL             :: FieldPat        -> f FieldPat,           
        bodyPL                 :: Body            -> f Body,               
        expPL                  :: Exp             -> f Exp,                
        fieldExpPL             :: FieldExp        -> f FieldExp,           
        guardedExpPL           :: GuardedExp      -> f GuardedExp,         
        matchPL                :: Match           -> f Match,              
        stmtPL                 :: Stmt            -> f Stmt,               
        rangePL                :: Range           -> f Range,              
        typPL                  :: Type            -> f Type,               
        tyVarBndrPL            :: TyVarBndr       -> f TyVarBndr,          
        kindPL                 :: Kind            -> f Kind,               
        cxtPL                  :: Cxt             -> f Cxt,                
        predPL                 :: Pred            -> f Pred,               
        foreignPL              :: Foreign         -> f Foreign,            
        pragmaPL               :: Pragma          -> f Pragma,             
        guardPL                :: Guard           -> f Guard,              
        litPL                  :: Lit             -> f Lit,                
        famFlavourPL           :: FamFlavour      -> f FamFlavour,         
        namePL                 :: Name            -> f Name,               
        callConvPL             :: Callconv        -> f Callconv,           
        safetyPL               :: Safety          -> f Safety,             
        inlineSpecPL           :: InlineSpec      -> f InlineSpec,         
        funDepPL               :: FunDep          -> f FunDep,             
        strictPL               :: Strict          -> f Strict,             
        fixityPL               :: Fixity          -> f Fixity,             
        fixityDirectionPL      :: FixityDirection -> f FixityDirection     
    }                                                                      
                                                                           
instance Multiplate THPlate where                                          
    multiplate p = THPlate buildInfo 
                           buildDec 
                           buildClause     
                           buildCon 
                           buildStrictType 
                           buildVarStrictType 
                           buildPat 
                           buildFieldPat 
                           buildBody       
                           buildExp   
                           buildFieldExp 
                           buildGuardedExp
                           buildMatch 
                           buildStmt 
                           buildRange      
                           buildTyp  
                           buildTyVarBndr  
                           buildKind 
                           buildCxt 
                           buildPred       
                           buildForeign 
                           buildPragma 
                           buildGuard    
                           buildLit            
                           buildFamFlavour     
                           buildName           
                           buildCallconv       
                           buildSafety         
                           buildInlineSpec     
                           buildFunDep         
                           buildStrict         
                           buildFixity         
                           buildFixityDirection where
          buildInfo   (ClassI d ds	     ) = ClassI  
                                          <$> decPL p d 
                                          <*> traverse (decPL p) ds
                                                        
          buildInfo   (ClassOpI nx t ny f) = ClassOpI   
                                          <$> namePL   p nx 
                                          <*> typPL    p t             
                                          <*> namePL   p ny  
                                          <*> fixityPL p f
                                                                      
          buildInfo   (TyConI d	         ) = TyConI  
                                          <$> decPL p d
                                                        
          buildInfo   (FamilyI d ds      ) = FamilyI 
                                          <$> decPL p d 
                                          <*> traverse (decPL p) ds
                                                                      
          buildInfo   (PrimTyConI n i b	 ) = PrimTyConI 
                                          <$> namePL p n  
                                          <*> pure   i              
                                          <*> pure   b  
                                                        
          buildInfo   (DataConI nx t ny f) = DataConI   
                                          <$> namePL   p nx 
                                          <*> typPL    p t               
                                          <*> namePL   p ny        
                                          <*> fixityPL p f
                                                        
          buildInfo   (VarI n t md f     ) = VarI       
                                          <$> namePL p n  
                                          <*> typPL p t               
                                          <*> sfmap (decPL p) md 
                                          <*> fixityPL p f
                                                        
          buildInfo   (TyVarI n t        ) = TyVarI     
                                          <$> namePL p n  
                                          <*> typPL p t              
          ---------------------------------------------------------------------      
          buildDec  (FunD n cs	                ) = FunD         
                                                 <$> namePL p n                              
                                                 <*> traverse (clausePL p) cs   
                                                               	                           
          buildDec  (ValD ptr b ds	            ) = ValD           
                                                 <$> patPL  p ptr                            
                                                 <*> bodyPL p b                           
                                                 <*> traverse (decPL p) ds
                                                                 	                  
          buildDec  (DataD ct n ts cs ns	   ) = DataD        
                                                <$> cxtPL  p ct                                  
                                                <*> namePL p n                                   
                                                <*> traverse (tyVarBndrPL p) ts                
                                                <*> traverse (conPL       p) cs                       
                                                <*> traverse (namePL      p) ns
                                                                                 
          buildDec  (NewtypeD ct n ts c ns	   ) = NewtypeD     
                                                <$> cxtPL  p ct                                 
                                                <*> namePL p n                                   
                                                <*> traverse (tyVarBndrPL p) ts                
                                                <*> conPL  p c                                   
                                                <*> traverse (namePL      p) ns 
                                                                      
          buildDec  (TySynD n ts t	           ) = TySynD       
                                                <$> namePL p n                                     
                                                <*> traverse (tyVarBndrPL p) ts                
                                                <*> typPL  p t                  
                                                                 	                 
          buildDec  (ClassD ct n ts fs ds	  ) = ClassD       
                                               <$> cxtPL  p ct                                 
                                               <*> namePL p n                                   
                                               <*> traverse (tyVarBndrPL p) ts                
                                               <*> traverse (funDepPL    p) fs                      
                                               <*> traverse (decPL       p) ds    
                                                                    
          buildDec  (InstanceD ct t decPLs	    ) = InstanceD    
                                                 <$> cxtPL p ct                               
                                                 <*> typPL p t                                	          
                                                 <*> traverse (decPL p) decPLs
                                                                 
          buildDec  (SigD n t	                ) = SigD                        
                                                 <$> namePL p n                	                      
                                                 <*> typPL  p t
                                                                 
          buildDec  (ForeignD x	                ) = ForeignD    
                                                 <$> foreignPL p x	                  
                                                 
          buildDec  (PragmaD x	                ) = PragmaD      
                                                 <$> pragmaPL  p x
                                                 
          buildDec  (FamilyD famFlav n tys mk) = FamilyD      
                                              <$> famFlavourPL p famFlav                      
                                              <*> namePL       p n                                   
                                              <*> traverse (tyVarBndrPL p) tys                
                                              <*> sfmap (kindPL p) mk    
                                                                      
          buildDec  (DataInstD    ct n  ts cs ns) = DataInstD    
                                                 <$> cxtPL  p ct                                
                                                 <*> namePL p n                                 
                                                 <*> traverse (typPL  p) ts                
                                                 <*> traverse (conPL  p) cs                    
                                                 <*> traverse (namePL p) ns   
                                                                            	      
          buildDec  (NewtypeInstD ct nx ts c nys) = NewtypeInstD 
                                                 <$> cxtPL  p ct                           
                                                 <*> namePL p nx                           
                                                 <*> traverse (typPL  p) ts                
                                                 <*> conPL  p c                            
                                                 <*> traverse (namePL p) nys
                                                  
          buildDec  (TySynInstD n ts t          ) = TySynInstD       
                                                 <$> namePL p n           
                                                 <*> traverse (typPL p) ts                    
                                                 <*> typPL  p t                            
-------------------------------------------------------------------------------
          buildClause (Clause ps bdy ds) = Clause <$> traverse (patPL p) ps 
                                                  <*> bodyPL p bdy 
                                                  <*> traverse (decPL p) ds
-------------------------------------------------------------------------------
          buildCon (NormalC n   sts     ) = NormalC 
                                         <$> namePL p n 
                                         <*> traverse (strictTypePL p) sts
                                                    
          buildCon (RecC    n   vsts    ) = RecC    
                                         <$> namePL p n                       
                                         <*> traverse (varStrictTypePL p) vsts
                                                    
          buildCon (InfixC  stx n    sty) = InfixC  
                                         <$> strictTypePL p stx                 
                                         <*> namePL       p n                         
                                         <*> strictTypePL p sty
                                                       
          buildCon (ForallC tys ct   c  ) = ForallC 
                                         <$> traverse (tyVarBndrPL p) tys  
                                         <*> cxtPL p ct                       
                                         <*> conPL p c
-------------------------------------------------------------------------------
          buildStrictType       (s, t) = (,)                 
                                      <$> strictPL p s 
                                      <*> typPL p t
-------------------------------------------------------------------------------
          buildVarStrictType (n, s, t) = (,,) <$> namePL   p n 
                                              <*> strictPL p s 
                                              <*> typPL    p t
-------------------------------------------------------------------------------
          buildPat (LitP x	            ) = LitP          
                                         <$> litPL p x
                                         
          buildPat (VarP x	            ) = VarP          
                                         <$> namePL p x
                                         
          buildPat (TupP ps	            ) = TupP          
                                         <$> traverse (patPL p) ps
                                         
          buildPat (UnboxedTupP ps	    ) = UnboxedTupP   
                                         <$> traverse (patPL p) ps
                                         
          buildPat (ConP n ps	        ) = ConP             
                                         <$> namePL p n                             
                                         <*> traverse (patPL p) ps 
                                                         
          buildPat (InfixP px n py      ) = InfixP                    
                                         <$> patPL  p px                               
                                         <*> namePL p n                 
                                         <*> patPL  p py 
                                                         
          buildPat (UInfixP px n py     ) = UInfixP                   
                                         <$> patPL  p px                              
                                         <*> namePL p n                
                                         <*> patPL  p py                
                                         
          buildPat (ParensP x	        ) = ParensP       
                                         <$> patPL p x
                                         
          buildPat (TildeP x	        ) = TildeP        
                                         <$> patPL p x
                                         
          buildPat (BangP x	            ) = BangP         
                                         <$> patPL p x
                                         
          buildPat (AsP n px	        ) = AsP           
                                         <$> namePL p n              
                                         <*> patPL  p px
                                         
          buildPat (WildP	            ) = pure WildP	            
          buildPat (RecP n fps          ) = RecP          
                                         <$> namePL p n                                 
                                         <*> traverse (fieldPatPL p) fps                
                                         
          buildPat (ListP ps	        ) = ListP         
                                         <$> traverse (patPL p) ps
                                                
          buildPat (SigP px t   	    ) = SigP          
                                         <$> patPL p px                
                                         <*> typPL p t
                                         
          buildPat (ViewP e px	        ) = ViewP                         
                                         <$> expPL p e 
                                         <*> patPL p px                	    
-------------------------------------------------------------------------------
          buildFieldPat (n, px) = (,) <$> namePL p n <*> patPL p px 
-------------------------------------------------------------------------------
          buildBody   (GuardedB ges ) = GuardedB    
                                     <$> traverse (guardedExpPL p) ges
          buildBody   (NormalB  e   ) = NormalB     
                                     <$> expPL p e
-------------------------------------------------------------------------------
          buildExp (VarE x          ) = VarE        
                                     <$> namePL p x
          
          buildExp (ConE x   	    ) = ConE        
                                     <$> namePL p x                       
          
          buildExp (LitE x	        ) = LitE        
                                     <$> litPL p x
          
          buildExp (AppE ex ey	    ) = AppE                      
                                     <$> expPL p ex                                  
                                     <*> expPL p ey 
                                                    
          buildExp (InfixE mex e mey) = InfixE             
                                     <$> sfmap (expPL p) mex                   
                                     <*> expPL p e                         
                                     <*> sfmap (expPL p) mey 
                                                   
          buildExp (UInfixE x y z	) = UInfixE                    
                                     <$> expPL p x                            
                                     <*> expPL p y              	             
                                     <*> expPL p z 
                                                    
          buildExp (ParensE e	    ) = ParensE     
                                     <$> expPL p e	  
                             
          buildExp (LamE ps e	    ) = LamE            
                                     <$> traverse (patPL p) ps                       
                                     <*> expPL p e
                                                              
          buildExp (TupE es	        ) = TupE        
                                     <$> traverse (expPL p) es
          	                     
          buildExp (UnboxedTupE es	) = UnboxedTupE 
                                     <$> traverse (expPL p) es
          	             
          buildExp (CondE x y z	    ) = CondE       
                                     <$> expPL p x                
                                     <*> expPL p y              
                                     <*> expPL p z	             
                                                    
          buildExp (LetE ds e	    ) = LetE        
                                     <$> traverse (decPL p) ds    
                                     <*> expPL p e	 
                                                                    
          buildExp (CaseE e ms	    ) = CaseE         
                                     <$> expPL p e                            
                                     <*> traverse (matchPL p) ms              
                                     
          buildExp (DoE ss	        ) = DoE         
                                     <$> traverse (stmtPL p) ss	                     
                                     
          buildExp (CompE ss	    ) = CompE       
                                     <$> traverse (stmtPL p) ss
                                                 
          buildExp (ArithSeqE r	    ) = ArithSeqE   
                                     <$> rangePL p r	                 
                                     
          buildExp (ListE es	    ) = ListE       
                                     <$> traverse (expPL p) es	                     
                                     
          buildExp (SigE e t	    ) = SigE        
                                     <$> expPL p e                
                                     <*> typPL p t	
                                                                     
          buildExp (RecConE n fs	) = RecConE     
                                     <$> namePL p n                 
                                     <*> traverse (fieldExpPL p) fs
                                     
          buildExp (RecUpdE e fs	) = RecUpdE     
                                     <$> expPL p e                
                                     <*> traverse (fieldExpPL p) fs
-------------------------------------------------------------------------------
          buildFieldExp   (n, e) = (,) <$> namePL  p n <*> expPL p e
          buildGuardedExp (g, e) = (,) <$> guardPL p g <*> expPL p e    
-------------------------------------------------------------------------------
          buildMatch (Match px b ds) = Match 
                                    <$> patPL  p px 
                                    <*> bodyPL p b 
                                    <*> traverse (decPL p) ds
-------------------------------------------------------------------------------
          buildStmt (BindS   px e) = BindS   
                                  <$> patPL p px 
                                  <*> expPL p e
                                  
          buildStmt (LetS    ds  ) = LetS    
                                  <$> traverse (decPL p) ds   
                                  
          buildStmt (NoBindS e   ) = NoBindS 
                                  <$> expPL p e  
                                  
          buildStmt (ParS    sss ) = ParS    
                                  <$> traverse (traverse (stmtPL p)) sss
-------------------------------------------------------------------------------
          buildRange (FromR x	       ) = FromR       
                                        <$> expPL p x
                                        	            
          buildRange (FromThenR x y	   ) = FromThenR   
                                        <$> expPL p x 
                                        <*> expPL p y
                                            
          buildRange (FromToR x y	   ) = FromToR     
                                        <$> expPL p x 
                                        <*> expPL p y    
          buildRange (FromThenToR x y z) = FromThenToR 
                                        <$> expPL p x 
                                        <*> expPL p y 
                                        <*> expPL p z
-------------------------------------------------------------------------------
          buildTyp (ForallT tys ct t ) = ForallT     
                                      <$> traverse (tyVarBndrPL p) tys 
                                      <*> cxtPL p ct 
                                      <*> typPL p t
                                      
          buildTyp (VarT n	         ) = VarT        
                                      <$> namePL p n
                                      	                
          buildTyp (ConT n	         ) = ConT        
                                      <$> namePL p n
                                      
          buildTyp x@(TupleT _	     ) = pure x      
          buildTyp x@(UnboxedTupleT _) = pure x	      
          buildTyp (ArrowT	         ) = pure ArrowT                        
          buildTyp (ListT	         ) = pure ListT	                    
          buildTyp (AppT tx ty	     ) = AppT        
                                      <$> typPL p tx                   
                                      <*> typPL p ty	    
                                              
          buildTyp (SigT t k         ) = SigT        
                                      <$> typPL  p t                   
                                      <*> kindPL p k              
-------------------------------------------------------------------------------
          buildTyVarBndr (PlainTV n   ) = PlainTV  
                                       <$> namePL p n
                                       
          buildTyVarBndr (KindedTV n k) = KindedTV 
                                       <$> namePL p n 
                                       <*> kindPL p k
-------------------------------------------------------------------------------
          buildKind StarK        = pure StarK
          buildKind (ArrowK x y) = ArrowK <$> kindPL p x <*> kindPL p y
-------------------------------------------------------------------------------
          buildCxt  ps = traverse (predPL p) ps
-------------------------------------------------------------------------------
          buildPred (ClassP n  ts) = ClassP 
                                  <$> namePL p n 
                                  <*> traverse (typPL p) ts
                                  
          buildPred (EqualP tx ty) = EqualP 
                                  <$> typPL p tx 
                                  <*> typPL p ty  
-------------------------------------------------------------------------------
          buildForeign (ImportF cc s x n t) = ImportF 
                                           <$> callConvPL p cc 
                                           <*> safetyPL   p s 
                                           <*> pure x   
                                           <*> namePL     p n 
                                           <*> typPL      p t
                                           
          buildForeign (ExportF cc   x n t) = ExportF 
                                           <$> callConvPL p cc 
                                           <*> pure x     
                                           <*> namePL     p n 
                                           <*> typPL      p t       
 
          buildPragma (InlineP n i       ) = InlineP     
                                          <$> namePL       p n 
                                          <*> inlineSpecPL p i
                                           
          buildPragma (SpecialiseP n t mi) = SpecialiseP 
                                          <$> namePL p n 
                                          <*> typPL  p t                              
                                          <*> sfmap (inlineSpecPL p) mi               
-------------------------------------------------------------------------------
          buildGuard (NormalG e) = NormalG <$> expPL p e
          buildGuard (PatG   ss) = PatG    <$> traverse (stmtPL p) ss
-------------------------------------------------------------------------------
          buildLit             x = pure x 
          buildFamFlavour      x = pure x
          buildName            x = pure x  
          buildCallconv        x = pure x
          buildSafety          x = pure x
          buildInlineSpec      x = pure x
          buildFunDep          x = pure x
          buildStrict          x = pure x
          buildFixity          x = pure x
          buildFixityDirection x = pure x
-------------------------------------------------------------------------------
          sfmap = (sequenceA .) . fmap

    mkPlate build = THPlate (build infoPL           )
                            (build decPL            )
                            (build clausePL         )
                            (build conPL            )
                            (build strictTypePL     )
                            (build varStrictTypePL  )
                            (build patPL            )
                            (build fieldPatPL       )
                            (build bodyPL           )
                            (build expPL            )
                            (build fieldExpPL       )
                            (build guardedExpPL     )
                            (build matchPL          )
                            (build stmtPL           )
                            (build rangePL          )
                            (build typPL            )
                            (build tyVarBndrPL      )
                            (build kindPL           )
                            (build cxtPL            )
                            (build predPL           )
                            (build foreignPL        )
                            (build pragmaPL         )
                            (build guardPL          )
                            (build litPL            )
                            (build famFlavourPL     )
                            (build namePL           )
                            (build callConvPL       )
                            (build safetyPL         )
                            (build inlineSpecPL     )
                            (build funDepPL         )
                            (build strictPL         )
                            (build fixityPL         )
                            (build fixityDirectionPL)