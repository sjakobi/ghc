-- | Types and functions for raw and lexed docstrings.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

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

  , DocIdEnv
  , hsDocIdentifierIdEnv

  , HsDoc'(..)

  , DocStructureItem(..)
  , DocStructure

  , Docs(..)
  ) where

#include "HsVersions.h"

import GhcPrelude

import Avail
import Binary
import Encoding
import FastFunctions
import Module
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

splitHsDoc :: HsDoc Name -> (DocIdEnv, HsDoc')
splitHsDoc (HsDoc s ids) = (id_env, hsDoc')
  where
    hsDoc' = HsDoc' s (hsDocIdentifierSpan <$> ids)
    id_env = foldMap hsDocIdentifierIdEnv ids

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

-- | Identifiers and the names they may correspond to.
type DocIdEnv = Map HsDocString [Name]

hsDocIdentifierIdEnv :: HsDocIdentifier Name -> DocIdEnv
hsDocIdentifierIdEnv (HsDocIdentifier _span s names) =
  Map.singleton s names

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

-- | A simplified version of 'HsImpExp.IE'.
data DocStructureItem
  = DsiSectionHeading Int HsDoc'
  | DsiDocChunk HsDoc'
  | DsiNamedChunkRef String
  | DsiExports Avails
  | DsiModExport ModuleName

instance Binary DocStructureItem where
  put_ bh = \case
    DsiSectionHeading level doc -> do
      putByte bh 0
      put_ bh level
      put_ bh doc
    DsiDocChunk doc -> do
      putByte bh 1
      put_ bh doc
    DsiNamedChunkRef name -> do
      putByte bh 2
      put_ bh name
    DsiExports avails -> do
      putByte bh 3
      put_ bh avails
    DsiModExport mod_name -> do
      putByte bh 4
      put_ bh mod_name

  get bh = do
    tag <- getByte bh
    case tag of
      0 -> DsiSectionHeading <$> get bh <*> get bh
      1 -> DsiDocChunk <$> get bh
      2 -> DsiNamedChunkRef <$> get bh
      3 -> DsiExports <$> get bh
      4 -> DsiModExport <$> get bh
      _ -> fail "instance Binary DocStructureItem: Invalid tag"

instance Outputable DocStructureItem where
  ppr = \case
    DsiSectionHeading level doc -> vcat
      [ text "section heading, level" <+> ppr level Outputable.<> colon
      , nest 2 (ppr doc)
      ]
    DsiDocChunk doc -> vcat
      [ text "documentation chunk:"
      , nest 2 (ppr doc)
      ]
    DsiNamedChunkRef name ->
      text "reference to named chunk:" <+> text name
    DsiExports avails ->
      text "avails:" $$ nest 2 (ppr avails)
    DsiModExport mod_name ->
      text "re-exported module:" <+> ppr mod_name

type DocStructure = [DocStructureItem]

data Docs = Docs
  { docs_id_env       :: DocIdEnv
    -- ^ Identifiers and the names they may correspond to.
  , docs_mod_hdr      :: Maybe HsDoc'
    -- ^ Module header.
  , docs_decls        :: Map Name HsDoc'
    -- ^ Docs for declarations: functions, data types, instances, methods etc.
  , docs_args         :: Map Name (Map Int HsDoc')
    -- ^ Docs for arguments. E.g. function arguments, method arguments.
  , docs_structure    :: DocStructure
  , docs_named_chunks :: Map String HsDoc'
    -- ^ Map from chunk name to content.
  }

instance Binary Docs where
  put_ bh docs = do
    put_ bh (docs_id_env docs)
    put_ bh (docs_mod_hdr docs)
    put_ bh (docs_decls docs)
    put_ bh (docs_args docs)
    put_ bh (docs_structure docs)
    put_ bh (docs_named_chunks docs)
  get bh = do
    id_env <- get bh
    mod_hdr <- get bh
    decls <- get bh
    args <- get bh
    structure <- get bh
    named_chunks <- get bh
    pure (Docs id_env mod_hdr decls args structure named_chunks)

instance Outputable Docs where
  ppr docs =
      vcat
        [ pprField (pprMap ppr (vcat . map pprName')) "identifier env"
                   docs_id_env
        , pprField ppr "module header" docs_mod_hdr
        , pprField (pprMap ppr ppr) "declaration docs" docs_decls
        , pprField (pprMap ppr (pprMap ppr ppr)) "arg docs" docs_args
        , pprField (vcat . map ppr) "documentation structure" docs_structure
        , pprField (pprMap (doubleQuotes . text) ppr) "named chunks"
                   docs_named_chunks
        ]
    where
      pprField ppr' heading lbl =
        text heading Outputable.<> colon $$ nest 2 (ppr' (lbl docs))
      pprMap pprKey pprVal m =
        vcat $ flip map (Map.toList m) $ \(k, v) ->
          pprKey k Outputable.<> colon $$ nest 2 (pprVal v)
      pprName' n =
        -- TODO: This looks a bit awkward.
        ppr (nameOccName n) <+> text "from" <+> ppr (nameModule n)
