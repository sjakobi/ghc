{-# LANGUAGE BangPatterns, CPP, DeriveDataTypeable #-}

module HsDoc (
  HsDoc(..),
  LHsDoc(..),
  HsDocString(..),
  LHsDocString,
  ppr_mbDoc
  ) where

#include "HsVersions.h"

import GhcPrelude

import Outputable
import SrcLoc
import FastString

import Data.Data

data HsDocIdentifierSpan = HsDocIdentifierSpan !Int !Int
  deriving (Eq, Show)

data HsDocIdentifier name = HsDocIdentifier
  { hsDocIdentifierSpan :: HsDocIdentifierSpan
  , hsDocIdentifierString :: HsDocString
  , hsDocIdentifierNames :: [name]
  } deriving (Eq, Show)

data HsDoc name = HsDoc
  { hsDocString :: HsDocString
  , hsDocIdentifiers :: [HsDocIdentifier name]
  } deriving (Eq, Show)

type LHsDoc name = Located (HsDoc name)

-- | Haskell Documentation String
newtype HsDocString = HsDocString FastString
  deriving (Eq, Show, Data)

-- | Located Haskell Documentation String
type LHsDocString = Located HsDocString

instance Outputable HsDocString where
  ppr (HsDocString fs) = ftext fs

ppr_mbDoc :: Maybe LHsDocString -> SDoc
ppr_mbDoc (Just doc) = ppr doc
ppr_mbDoc Nothing    = empty

