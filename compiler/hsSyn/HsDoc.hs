-- | Types and functions for raw and lexed docstrings.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module HsDoc
  ( HsDoc(..)
  , concatHsDoc
  , splitHsDoc
  , LHsDoc
  , ppr_mbDoc

  , HsDocString(..)
  , mkHsDocString
  , unpackHDS
  , mkHsDocStringUtf8ByteString
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

  , HaddockItem(..)
  ) where

#include "HsVersions.h"

import GhcPrelude

import Avail
import Module
import Name
import Outputable hiding ((<>))
import qualified Outputable
import SrcLoc
import Binary
import Encoding
import FastFunctions

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString.Internal as BS
import Data.Data
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup
import GHC.ForeignPtr
import GHC.Ptr
import Foreign

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

-- | Non-empty docstrings are joined with two newlines in between,
-- so haddock will treat two joined docstrings as separate paragraphs.
instance Semigroup (HsDoc a) where
  x               <> HsDoc s_y [] | nullHDS s_y = x
  HsDoc s_x []    <> y            | nullHDS s_x = y
  HsDoc s_x ids_x <> HsDoc s_y ids_y =
    HsDoc (s_x <> HsDocString (C8.pack "\n\n") <> s_y)
          (ids_x ++ map (shiftHsDocIdentifier (lengthHDS s_x + 2)) ids_y)

instance Monoid (HsDoc a) where
  mempty = HsDoc mempty []
  mappend = (<>)

-- | Concatenate several 'HsDoc's.
--
-- Returns 'Nothing' if all inputs are empty.
concatHsDoc :: [HsDoc name] -> Maybe (HsDoc name)
concatHsDoc xs =
  case mconcat xs of
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
-- Internally this is a UTF8-encoded 'ByteString'.
newtype HsDocString = HsDocString ByteString
  deriving (Eq, Ord, Show, Data, Semigroup, Monoid)

instance Binary HsDocString where
  put_ bh (HsDocString bs) = put_ bh bs
  get bh = HsDocString <$> get bh

instance Outputable HsDocString where
  ppr x = char '"' Outputable.<> text (unpackHDS x) Outputable.<> char '"'

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

data HaddockItem
  = HaddockSection Int HsDoc'
  | HaddockDoc HsDoc'
  | HaddockDocNamed String
  | HaddockAvails Avails
  | HaddockModule ModuleName

instance Binary HaddockItem where
  put_ bh = \case
    HaddockSection level doc -> do
      putByte bh 0
      put_ bh level
      put_ bh doc
    HaddockDoc doc -> do
      putByte bh 1
      put_ bh doc
    HaddockDocNamed name -> do
      putByte bh 2
      put_ bh name
    HaddockAvails avails -> do
      putByte bh 3
      put_ bh avails
    HaddockModule mod_name -> do
      putByte bh 4
      put_ bh mod_name

  get bh = do
    tag <- getByte bh
    case tag of
      0 -> HaddockSection <$> get bh <*> get bh
      1 -> HaddockDoc <$> get bh
      2 -> HaddockDocNamed <$> get bh
      3 -> HaddockAvails <$> get bh
      4 -> HaddockModule <$> get bh
      _ -> fail "instance Binary HaddockItem: Invalid tag"

instance Outputable HaddockItem where
  ppr = \case
    HaddockSection level doc -> vcat
      [ text "section heading, level" <+> ppr level Outputable.<> colon
      , nest 2 (ppr doc)
      ]
    HaddockDoc doc -> vcat
      [ text "documentation chunk:"
      , nest 2 (ppr doc)
      ]
    HaddockDocNamed name ->
      text "reference to named chunk:" <+> text name
    HaddockAvails avails ->
      text "avails:" $$ nest 2 (ppr avails)
    HaddockModule mod_name ->
      text "re-exported module:" <+> ppr mod_name
