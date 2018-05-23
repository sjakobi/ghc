{-# language TypeFamilies #-}
module SeparateDocs where

import GhcPrelude
import Bag
import HsBinds
import HsDoc
import HsDecls
import HsExtension
import Name
import SrcLoc

import Control.Applicative
import Data.List
import Data.Ord

-- | The top-level declarations of a module that we care about,
-- ordered by source location, with documentation attached if it exists.
topDecls :: HsGroup GhcRn -> [(LHsDecl GhcRn, [HsDoc Name])]
topDecls = filterClasses . filterDecls . collectDocs . sortByLoc . ungroup

-- | Take all declarations except pragmas, infix decls, rules from an 'HsGroup'.
ungroup :: HsGroup GhcRn -> [LHsDecl GhcRn]
ungroup group_ =
  mkDecls (tyClGroupTyClDecls . hs_tyclds) (TyClD noExt)  group_ ++
  mkDecls hs_derivds             (DerivD noExt) group_ ++
  mkDecls hs_defds               (DefD noExt)   group_ ++
  mkDecls hs_fords               (ForD noExt)   group_ ++
  mkDecls hs_docs                (DocD noExt)   group_ ++
  mkDecls (tyClGroupInstDecls . hs_tyclds) (InstD noExt)  group_ ++
  mkDecls (typesigs . hs_valds)  (SigD noExt)   group_ ++
  mkDecls (valbinds . hs_valds)  (ValD noExt)   group_
  where
    typesigs (XValBindsLR (NValBinds _ sigs)) = filter (isUserSig . unLoc) sigs
    typesigs _ = error "expected ValBindsOut"

    valbinds (XValBindsLR (NValBinds binds _)) = concatMap bagToList . snd . unzip $ binds
    valbinds _ = error "expected ValBindsOut"

-- | Sort by source location
sortByLoc :: [Located a] -> [Located a]
sortByLoc = sortBy (comparing getLoc)

-- | Collect docs and attach them to the right declarations.
collectDocs :: [LHsDecl pass] -> [(LHsDecl pass, [HsDoc (IdP pass)])]
collectDocs = go Nothing []
  where
    go Nothing _ [] = []
    go (Just prev) docs [] = finished prev docs []
    go prev docs (L _ (DocD _ (DocCommentNext str)) : ds)
      | Nothing <- prev = go Nothing (str:docs) ds
      | Just decl <- prev = finished decl docs (go Nothing [str] ds)
    go prev docs (L _ (DocD _ (DocCommentPrev str)) : ds) = go prev (str:docs) ds
    go Nothing docs (d:ds) = go (Just d) docs ds
    go (Just prev) docs (d:ds) = finished prev docs (go (Just d) [] ds)

    finished decl docs rest = (decl, reverse docs) : rest

-- | Filter out declarations that we don't handle in Haddock
filterDecls :: [(LHsDecl a, doc)] -> [(LHsDecl a, doc)]
filterDecls = filter (isHandled . unLoc . fst)
  where
    isHandled (ForD _ (ForeignImport {})) = True
    isHandled (TyClD {})  = True
    isHandled (InstD {})  = True
    isHandled (DerivD {}) = True
    isHandled (SigD _ d)  = isUserSig d
    isHandled (ValD {})   = True
    -- we keep doc declarations to be able to get at named docs
    isHandled (DocD {})   = True
    isHandled _ = False


-- | Go through all class declarations and filter their sub-declarations
filterClasses :: [(LHsDecl a, doc)] -> [(LHsDecl a, doc)]
filterClasses decls = [ if isClassD d then (L loc (filterClass d), doc) else x
                      | x@(L loc d, doc) <- decls ]
  where
    filterClass (TyClD x c) =
      TyClD x $ c { tcdSigs = filter (liftA2 (||) (isUserSig . unLoc) isMinimalLSig) $ tcdSigs c }
    filterClass _ = error "expected TyClD"

-- | Was this signature given by the user?
isUserSig :: Sig name -> Bool
isUserSig TypeSig {}    = True
isUserSig ClassOpSig {} = True
isUserSig PatSynSig {}  = True
isUserSig _             = False

isClassD :: HsDecl a -> Bool
isClassD (TyClD _ d) = isClassDecl d
isClassD _ = False

-- | Take a field of declarations from a data structure and create HsDecls
-- using the given constructor
mkDecls :: (a -> [Located b]) -> (b -> c) -> a -> [Located c]
mkDecls field con struct = [ L loc (con decl) | L loc decl <- field struct ]
