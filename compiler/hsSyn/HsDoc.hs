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
  HsDoc'(..)
  ) where

#include "HsVersions.h"

import GhcPrelude

import Name
import Outputable
import SrcLoc
import FastString

import Data.Data
import Data.Map (Map)

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
  deriving (Eq, Show, Data)

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

-- | A version of 'HsDoc' intended for serialization.
data HsDoc' = HsDoc'
  { hsDoc'String :: !HsDocString
  , hsDoc'IdentifierSpans :: ![HsDocIdentifierSpan]
  }
