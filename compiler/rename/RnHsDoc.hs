
module RnHsDoc ( rnHsDoc, rnLHsDoc, rnMbLHsDoc ) where

import GhcPrelude

import TcRnTypes
import HsSyn
import SrcLoc
import RdrName
import Name


rnMbLHsDoc :: Maybe (LHsDoc RdrName) -> RnM (Maybe (LHsDoc Name))
rnMbLHsDoc = traverse rnLHsDoc

rnLHsDoc :: LHsDoc RdrName -> RnM (LHsDoc Name)
rnLHsDoc (L pos doc) = do
  doc' <- rnHsDoc doc
  return (L pos doc')

rnHsDoc :: HsDoc RdrName -> RnM (HsDoc Name)
rnHsDoc = undefined

