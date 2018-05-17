module RnHsDoc ( rnHaddock, rnHsDoc, rnLHsDoc, rnMbLHsDoc ) where

import GhcPrelude

import TcRnTypes
import TcRnMonad
import HsSyn
import RdrName
import Name
import RnEnv
import IOEnv

rnHaddock :: Maybe (LHsDoc RdrName) -> TcGblEnv -> RnM TcGblEnv
rnHaddock mb_doc tcg_env = do
  rn_doc <- rnMbLHsDoc mb_doc
  pure tcg_env { tcg_doc_hdr = rn_doc }

rnMbLHsDoc :: Maybe (LHsDoc RdrName) -> RnM (Maybe (LHsDoc Name))
rnMbLHsDoc = traverse rnLHsDoc

rnLHsDoc :: LHsDoc RdrName -> RnM (LHsDoc Name)
rnLHsDoc = traverse rnHsDoc

rnHsDoc :: HsDoc RdrName -> RnM (HsDoc Name)
rnHsDoc (HsDoc s ids) = HsDoc s <$> traverse rnHsDocIdentifier ids

rnHsDocIdentifier :: HsDocIdentifier RdrName -> RnM (HsDocIdentifier Name)
rnHsDocIdentifier (HsDocIdentifier span s rdr_names) = do
  -- TODO: Add a check that there should be exactly 1 RdrName in the identifier?

  -- Generate the choices for the possible kind of thing this
  -- is.
  let choices = concatMap dataTcOccs rdr_names

  gre <- tcg_rdr_env <$> getGblEnv
  -- Try to look up all the names in the GlobalRdrEnv that match
  -- the names.
  let names = concatMap (\c -> map gre_name (lookupGRE_RdrName c gre)) choices
  pure (HsDocIdentifier span s names)
