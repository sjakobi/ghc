-- | Types and functions for raw and lexed docstrings.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HsDoc
  ( HsDoc(..)
  , appendHsDoc
  , concatHsDoc
  , splitHsDoc
  , LHsDoc
  , ppr_mbDoc

  , HsDocString
  , mkHsDocString
  , mkHsDocStringUtf8ByteString
  , unpackHDS
  , hsDocStringToByteString
  , LHsDocString

  , HsDocIdentifier(..)

  , HsDocIdentifierSpan(..)

  , HsDocNamesMap(..)
  , emptyHsDocNamesMap
  , hsDocIdentifierNamesMap

  , HsDoc'(..)

  , DeclDocMap(..)
  , emptyDeclDocMap

  , ArgDocMap(..)
  , emptyArgDocMap
  ) where

#include "HsVersions.h"

import GhcPrelude

import Binary
import Encoding
import FastFunctions
import Name
import Outputable
import SrcLoc

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString.Internal as BS
import Data.Data
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Foreign
import GHC.ForeignPtr

-- | The location of an identifier in a 'HsDocString'.

-- TODO: This could be a newtype of Word64
data HsDocIdentifierSpan = HsDocIdentifierSpan
  { hsDocIdentifierSpanStart :: !Int
    -- ^ The position of the first character of the identifier.
  , hsDocIdentifierSpanEnd   :: !Int
    -- ^ The position of the first character after the identifier.
  } deriving (Eq, Show, Data)

instance Binary HsDocIdentifierSpan where
  put_ bh (HsDocIdentifierSpan a b) = do
    put_ bh a
    put_ bh b
  get bh = do
    a <- get bh
    b <- get bh
    return (HsDocIdentifierSpan a b)

instance Outputable HsDocIdentifierSpan where
  ppr (HsDocIdentifierSpan a b) =
    int a Outputable.<> char '-' Outputable.<> int b

shiftHsDocIdentifierSpan :: Int -> HsDocIdentifierSpan -> HsDocIdentifierSpan
shiftHsDocIdentifierSpan n (HsDocIdentifierSpan a b) =
  HsDocIdentifierSpan (a + n) (b + n)

-- | An identifier from a docstring.
data HsDocIdentifier name = HsDocIdentifier
  { hsDocIdentifierSpan :: !HsDocIdentifierSpan
  , hsDocIdentifierString :: !HsDocString -- ^ The text of the docstring.
  , hsDocIdentifierNames :: ![name]
  } deriving (Eq, Show, Data)

shiftHsDocIdentifier :: Int -> HsDocIdentifier name -> HsDocIdentifier name
shiftHsDocIdentifier n (HsDocIdentifier span s names) =
  HsDocIdentifier (shiftHsDocIdentifierSpan n span) s names

-- | A docstring with the (probable) identifiers found in it.
data HsDoc name = HsDoc
  { hsDocString :: !HsDocString
  , hsDocIdentifiers :: ![HsDocIdentifier name]
  } deriving (Eq, Show, Data)

instance Outputable (HsDoc a) where
  ppr _ = text "<document comment>"

emptyHsDoc :: HsDoc a
emptyHsDoc = HsDoc (HsDocString BS.empty) []

-- | Non-empty docstrings are joined with two newlines in between,
-- so haddock will treat two joined docstrings as separate paragraphs.
appendHsDoc :: HsDoc a -> HsDoc a -> HsDoc a
appendHsDoc x              (HsDoc s_y []) | nullHDS s_y = x
appendHsDoc (HsDoc s_x []) y              | nullHDS s_x = y
appendHsDoc (HsDoc s_x ids_x) (HsDoc s_y ids_y) =
  HsDoc (concatHDS [s_x, HsDocString (C8.pack "\n\n"), s_y])
        (ids_x ++ map (shiftHsDocIdentifier (lengthHDS s_x + 2)) ids_y)

-- | Concatenate several 'HsDoc's with 'appendHsDoc'.
--
-- Returns 'Nothing' if all inputs are empty.
concatHsDoc :: [HsDoc name] -> Maybe (HsDoc name)
concatHsDoc xs =
  -- Yes, this isn't particularly efficient but it's only used
  -- when we have to concat multiple doc comments for the same
  -- declaration which shouldn't happen too often.
  case foldl' appendHsDoc emptyHsDoc xs of
    HsDoc s [] | nullHDS s -> Nothing
    x -> Just x

splitHsDoc :: HsDoc Name -> (HsDocNamesMap, HsDoc')
splitHsDoc (HsDoc s ids) = (names, hsDoc')
  where
    hsDoc' = HsDoc' s (hsDocIdentifierSpan <$> ids)
    names = foldMap hsDocIdentifierNamesMap ids

type LHsDoc name = Located (HsDoc name)

ppr_mbDoc :: Maybe (LHsDoc a) -> SDoc
ppr_mbDoc (Just doc) = ppr doc
ppr_mbDoc Nothing    = empty

-- | Haskell Documentation String
--
-- Internally this is a UTF8-Encoded 'ByteString'.
newtype HsDocString = HsDocString ByteString
  -- There are at least two plausible Semigroup instances for this type:
  --
  -- 1. Simple string concatenation.
  -- 2. Concatenation as documentation paragraphs with newlines in between.
  --
  -- To avoid confusion, we pass on defining an instance at all.
  deriving (Eq, Ord, Show, Data)

instance Binary HsDocString where
  put_ bh (HsDocString bs) = put_ bh bs
  get bh = HsDocString <$> get bh

instance Outputable HsDocString where
  ppr = doubleQuotes . text . unpackHDS

mkHsDocString :: String -> HsDocString
mkHsDocString s =
  inlinePerformIO $ do
    let len = utf8EncodedLength s
    buf <- mallocForeignPtrBytes len
    withForeignPtr buf $ \ptr -> do
      utf8EncodeString ptr s
      pure (HsDocString (BS.fromForeignPtr buf 0 len))

-- | Create a 'HsDocString' from a UTF8-encoded 'ByteString'.
mkHsDocStringUtf8ByteString :: ByteString -> HsDocString
mkHsDocStringUtf8ByteString = HsDocString

unpackHDS :: HsDocString -> String
unpackHDS = utf8DecodeByteString . hsDocStringToByteString

-- | Return the contents of a 'HsDocString' as a UTF8-encoded 'ByteString'.
hsDocStringToByteString :: HsDocString -> ByteString
hsDocStringToByteString (HsDocString bs) = bs

nullHDS :: HsDocString -> Bool
nullHDS (HsDocString bs) = BS.null bs

lengthHDS :: HsDocString -> Int
lengthHDS (HsDocString (PS fptr off len)) =
  inlinePerformIO $
    countUTF8Chars (plusPtr (unsafeForeignPtrToPtr fptr) off) len

concatHDS :: [HsDocString] -> HsDocString
concatHDS = HsDocString . BS.concat . map hsDocStringToByteString

type LHsDocString = Located HsDocString

-- | A collection of identifiers.
newtype HsDocNamesMap = HsDocNamesMap (Map HsDocString [Name])
  deriving ( Semigroup
             -- ^ Assumes that equal identifiers will correspond to the same
             -- names.
           , Monoid
           )

instance Binary HsDocNamesMap where
  put_ bh (HsDocNamesMap m) = put_ bh (Map.toAscList m)
  get bh = HsDocNamesMap . Map.fromDistinctAscList <$> get bh

instance Outputable HsDocNamesMap where
  ppr (HsDocNamesMap m) = vcat (map pprPair (Map.toAscList m))
    where
      pprPair (s, names) =
        ppr s Outputable.<> colon $$ nest 2 (vcat (map pprName' names))
      pprName' n = ppr (nameOccName n) <+> text "from" <+> ppr (nameModule n)

emptyHsDocNamesMap :: HsDocNamesMap
emptyHsDocNamesMap = HsDocNamesMap Map.empty

hsDocIdentifierNamesMap :: HsDocIdentifier Name -> HsDocNamesMap
hsDocIdentifierNamesMap (HsDocIdentifier _span s names) =
  HsDocNamesMap (Map.singleton s names)

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

-- | Docs for declarations: functions, data types, instances, methods etc.
newtype DeclDocMap = DeclDocMap (Map Name HsDoc')

instance Binary DeclDocMap where
  put_ bh (DeclDocMap m) = put_ bh (Map.toAscList m)
  get bh = DeclDocMap . Map.fromDistinctAscList <$> get bh

instance Outputable DeclDocMap where
  ppr (DeclDocMap m) = vcat (map pprPair (Map.toAscList m))
    where
      pprPair (name, doc) = ppr name Outputable.<> colon $$ nest 2 (ppr doc)

emptyDeclDocMap :: DeclDocMap
emptyDeclDocMap = DeclDocMap Map.empty

-- | Docs for arguments. E.g. function arguments, method arguments.
newtype ArgDocMap = ArgDocMap (Map Name (Map Int HsDoc'))

instance Binary ArgDocMap where
  put_ bh (ArgDocMap m) = put_ bh (Map.toAscList (Map.toAscList <$> m))
  get bh = ArgDocMap . fmap Map.fromDistinctAscList . Map.fromDistinctAscList
             <$> get bh

instance Outputable ArgDocMap where
  ppr (ArgDocMap m) = vcat (map pprPair (Map.toAscList m))
    where
      pprPair (name, int_map) =
        ppr name Outputable.<> colon $$ nest 2 (pprIntMap int_map)
      pprIntMap im = vcat (map pprIPair (Map.toAscList im))
      pprIPair (i, doc) = ppr i Outputable.<> colon $$ nest 2 (ppr doc)

emptyArgDocMap :: ArgDocMap
emptyArgDocMap = ArgDocMap Map.empty
