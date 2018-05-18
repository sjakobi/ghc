{-# LANGUAGE BangPatterns, CPP, DeriveDataTypeable #-}

module HsDoc (
  HsDoc(..),
  LHsDoc,
  HsDocString(..),
  LHsDocString,
  mkHsDocString,
  HsDocIdentifier(..),
  HsDocIdentifierSpan(..),
  ppr_mbDoc,
  HsDocNamesMap(..),
  HsDoc'(..),
  combineDocs
  ) where

#include "HsVersions.h"

import GhcPrelude

import Name
import Outputable
import SrcLoc
import FastString

import Data.Data
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map

data HsDocIdentifierSpan = HsDocIdentifierSpan !Int !Int
  deriving (Eq, Show, Data)

data HsDocIdentifier name = HsDocIdentifier
  { hsDocIdentifierSpan :: !HsDocIdentifierSpan
  , hsDocIdentifierString :: !HsDocString
  , hsDocIdentifierNames :: ![name]
  } deriving (Eq, Show, Data)

data HsDoc name = HsDoc
  { hsDocString :: !HsDocString
  , hsDocIdentifiers :: ![HsDocIdentifier name]
  } deriving (Eq, Show, Data)
instance Outputable (HsDoc a) where
  ppr _ = text "<document comment>"

type LHsDoc name = Located (HsDoc name)

-- | Haskell Documentation String
newtype HsDocString = HsDocString FastString
  deriving (Eq, Ord, Show, Data)

-- | Located Haskell Documentation String
type LHsDocString = Located HsDocString

instance Outputable HsDocString where
  ppr (HsDocString fs) = ftext fs

mkHsDocString :: String -> HsDocString
mkHsDocString = HsDocString . mkFastString

ppr_mbDoc :: Maybe (LHsDoc a) -> SDoc
ppr_mbDoc (Just doc) = ppr doc
ppr_mbDoc Nothing    = empty

-- | The collected identifiers for a module.
newtype HsDocNamesMap = HsDocNamesMap (Map HsDocString [Name])

emptyHsDocNamesMap :: HsDocNamesMap
emptyHsDocNamesMap = HsDocNamesMap Map.empty

-- | Assumes that equal identifiers will correspond to the same names.
unionHsDocNamesMap :: HsDocNamesMap -> HsDocNamesMap -> HsDocNamesMap
unionHsDocNamesMap (HsDocNamesMap m0) (HsDocNamesMap m1) =
  HsDocNamesMap (Map.union m0 m1)

-- | A version of 'HsDoc' intended for serialization.
data HsDoc' = HsDoc'
  { hsDoc'String :: !HsDocString
  , hsDoc'IdentifierSpans :: ![HsDocIdentifierSpan]
  }

combineDocs :: Maybe (LHsDoc Name) -> (HsDocNamesMap, Maybe HsDoc')
combineDocs mb_doc_hdr = splitMbHsDoc (unLoc <$> mb_doc_hdr)

splitMbHsDoc :: Maybe (HsDoc Name) -> (HsDocNamesMap, Maybe HsDoc')
splitMbHsDoc Nothing = (emptyHsDocNamesMap, Nothing)
splitMbHsDoc (Just hsDoc) = Just <$> splitHsDoc hsDoc

splitHsDoc :: HsDoc Name -> (HsDocNamesMap, HsDoc')
splitHsDoc (HsDoc s ids) = (names, hsDoc')
  where
    hsDoc' = HsDoc' s (hsDocIdentifierSpan <$> ids)
    names = foldl' (\m id' -> unionHsDocNamesMap m (hsDocIdentifierNamesMap id'))
                   emptyHsDocNamesMap
                   ids

hsDocIdentifierNamesMap :: HsDocIdentifier Name -> HsDocNamesMap
hsDocIdentifierNamesMap (HsDocIdentifier _span s names) =
  HsDocNamesMap (Map.singleton s names)
