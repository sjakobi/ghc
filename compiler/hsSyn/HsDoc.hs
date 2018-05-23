{-# LANGUAGE BangPatterns, CPP, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module HsDoc (
  HsDoc(..),
  LHsDoc,
  concatHsDoc,
  HsDocString(..),
  LHsDocString,
  mkHsDocString,
  HsDocIdentifier(..),
  HsDocIdentifierSpan(..),
  ppr_mbDoc,
  HsDocNamesMap(..),
  emptyHsDocNamesMap,
  HsDoc'(..),
  combineDocs
  ) where

#include "HsVersions.h"

import GhcPrelude

import Name
import Outputable hiding ((<>))
import qualified Outputable
import SrcLoc
import FastString
import Binary

import Data.Data
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup

data HsDocIdentifierSpan = HsDocIdentifierSpan !Int !Int
  deriving (Eq, Show, Data)

instance Binary HsDocIdentifierSpan where
  put_ bh (HsDocIdentifierSpan a b) = do
    put_ bh a
    put_ bh b
  get bh = do
    a <- get bh
    b <- get bh
    return (HsDocIdentifierSpan a b)

instance Outputable HsDocIdentifierSpan where
  ppr (HsDocIdentifierSpan a b) = int a Outputable.<> char '-' Outputable.<> int b

shiftHsDocIdentifierSpan :: Int -> HsDocIdentifierSpan -> HsDocIdentifierSpan
shiftHsDocIdentifierSpan n (HsDocIdentifierSpan a b) =
  HsDocIdentifierSpan (a + n) (b + n)

data HsDocIdentifier name = HsDocIdentifier
  { hsDocIdentifierSpan :: !HsDocIdentifierSpan
  , hsDocIdentifierString :: !HsDocString
  , hsDocIdentifierNames :: ![name]
  } deriving (Eq, Show, Data)

shiftHsDocIdentifier :: Int -> HsDocIdentifier name -> HsDocIdentifier name
shiftHsDocIdentifier n (HsDocIdentifier span s names) =
  HsDocIdentifier (shiftHsDocIdentifierSpan n span) s names

data HsDoc name = HsDoc
  { hsDocString :: !HsDocString
  , hsDocIdentifiers :: ![HsDocIdentifier name]
  } deriving (Eq, Show, Data)

instance Outputable (HsDoc a) where
  ppr _ = text "<document comment>"

instance Semigroup (HsDoc a) where
  -- Do I need to add an extra '\n' in between?
  HsDoc s0 ids0 <> HsDoc s1 ids1 =
    HsDoc (s0 <> s1)
          (ids0 ++ map (shiftHsDocIdentifier (lengthHDS s0)) ids1)

instance Monoid (HsDoc a) where
  mempty = HsDoc mempty []
  mappend = (<>)

concatHsDoc :: [HsDoc name] -> Maybe (HsDoc name)
concatHsDoc xs =
  case mconcat xs of
    HsDoc s [] | nullHDS s -> Nothing
    x -> Just x

type LHsDoc name = Located (HsDoc name)

-- | Haskell Documentation String
newtype HsDocString = HsDocString FastString
  deriving (Eq, Ord, Show, Data, Semigroup, Monoid)

instance Binary HsDocString where
  put_ bh (HsDocString fs) = put_ bh fs
  get bh = HsDocString <$> get bh

nullHDS :: HsDocString -> Bool
nullHDS (HsDocString fs) = nullFS fs

lengthHDS :: HsDocString -> Int
lengthHDS (HsDocString fs) = lengthFS fs

-- | Located Haskell Documentation String
type LHsDocString = Located HsDocString

instance Outputable HsDocString where
  ppr (HsDocString fs) = char '"' Outputable.<> ftext fs Outputable.<> char '"'

mkHsDocString :: String -> HsDocString
mkHsDocString = HsDocString . mkFastString

ppr_mbDoc :: Maybe (LHsDoc a) -> SDoc
ppr_mbDoc (Just doc) = ppr doc
ppr_mbDoc Nothing    = empty

-- | The collected identifiers for a module.
newtype HsDocNamesMap = HsDocNamesMap (Map HsDocString [Name])

instance Binary HsDocNamesMap where
  put_ bh (HsDocNamesMap m) = put_ bh (Map.toAscList m)
  get bh = HsDocNamesMap . Map.fromDistinctAscList <$> get bh

instance Outputable HsDocNamesMap where
  ppr (HsDocNamesMap m) = vcat (map pprPair (Map.toAscList m))
    where
      pprPair (s, names) = ppr s Outputable.<> colon $$ nest 2 (vcat (map pprName' names))
      pprName' n = ppr (nameOccName n) <+> text "from" <+> ppr (nameModule n)

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

instance Binary HsDoc' where
  put_ bh (HsDoc' s spans) = do
    put_ bh s
    put_ bh spans
  get bh = do
    s <- get bh
    spans <- get bh
    return (HsDoc' s spans)

instance Outputable HsDoc' where
  ppr (HsDoc' s spans) =
    vcat [ text "text:" $$ nest 2 (ppr s)
         , text "spans:" <+> fsep (punctuate (char ',') (map ppr spans))
         ]

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
