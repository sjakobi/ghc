{-# language FlexibleContexts #-}

module HaddockLex (lexHsDoc) where

import GhcPrelude

import FastString
import HsDoc
import Lexer
import Module
import SrcLoc
import StringBuffer
import RdrName
import qualified EnumSet

import Control.Applicative
import Data.Char
import Data.Functor
import Data.Maybe

import Text.Parsec (Stream, ParsecT)
import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P

-- | Lex identifiers from a docstring.
lexHsDoc :: P (GenLocated loc RdrName) -- ^ A precise identifier parser
         -> String                     -- ^ A docstring
         -> HsDoc RdrName
lexHsDoc identParser s =
    HsDoc (mkHsDocString s) (mapMaybe maybeDocIdentifier plausibleIdents)
  where
    maybeDocIdentifier :: (Int, String, Int) -> Maybe (HsDocIdentifier RdrName)
    maybeDocIdentifier (ix0, pid, ix1) =
      HsDocIdentifier (HsDocIdentifierSpan ix0 ix1) (mkHsDocString pid) . (: [])
        <$> validateIdentWith identParser pid

    plausibleIdents :: [(Int, String, Int)]
    plausibleIdents =
      either (error . show)
             id
             (P.runParser (setOffset 0
                           *> identsWith (delimited (withOffsets plausibleIdent)))
                          () "" s)

identsWith :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
identsWith p =
    catMaybes
      <$> P.many (P.try (Just <$> p)
                  <|> handleNewline
                  <|> dropDelim
                  <|> dropUntilDelimOrNewline
                 )
  where
    -- Increment the offset (actually the column position) instead of having it
    -- to 0.
    handleNewline = do
      off <- getOffset
      _ <- P.char '\n'
      setOffset (succ off)
      pure Nothing

    dropDelim = delim $> Nothing

    dropUntilDelimOrNewline =
      P.skipMany1 (P.satisfy (\c -> not (isDelim c || c == '\n'))) $> Nothing

-- | An overly permissive identifier parser
plausibleIdent :: Stream s m Char => ParsecT s u m String
plausibleIdent = do
    c <- identStart
    cs <- p
    return (c : cs)
  where
    p = do
      vs <- many identExcept'
      c <- P.lookAhead P.anyChar
      case c of
        '`' -> return vs
        '\'' -> P.try ((\x -> vs ++ "'" ++ x) <$> (P.char '\'' *> p)) <|> return vs
        _ -> fail "Not a delimiter"

    identStart = P.letter <|> P.char '_' <|> symbol
    identExcept' = P.satisfy isAlphaNum <|> P.char '_' <|> symbol
    symbol = asciiSymbol <|> unicodeSymbol
    asciiSymbol = P.oneOf "!#$%&⋆+./<=>?@\\^|-~:"
    unicodeSymbol = P.satisfy (\c -> not (isAscii c) && isSymbol c)

withOffsets :: Stream s m Char => ParsecT s u m a -> ParsecT s u m (Int, a, Int)
withOffsets p = liftA3 (,,) getOffset p getOffset

delimited :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
delimited = P.between delim delim

delim :: Stream s m Char => ParsecT s u m Char
delim = P.satisfy isDelim

isDelim :: Char -> Bool
isDelim c = c == '\'' || c == '`'

validateIdentWith :: P (GenLocated loc RdrName) -> String -> Maybe RdrName
validateIdentWith identParser str0 =
  let pflags = ParserFlags EnumSet.empty EnumSet.empty (stringToUnitId "") 0
      buffer = stringToStringBuffer str0
      realSrcLc = mkRealSrcLoc (mkFastString "") 0 0
      pstate = mkPStatePure pflags buffer realSrcLc
  in case unP identParser pstate of
    POk _ name -> Just (unLoc name)
    _ -> Nothing

--------------------------------------------------------------------------------
-- Offset handling

-- We use parsec's 'P.getPosition' and 'P.setPosition' to track our offset
-- from the beginning of the stream.
setOffset :: Monad m => Int -> ParsecT s u m ()
setOffset off = P.setPosition (P.newPos "" 0 off)

getOffset :: Monad m => ParsecT s u m Int
getOffset = P.sourceColumn <$> P.getPosition
