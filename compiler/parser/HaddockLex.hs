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

-- | Lex identifiers from a docstring.
lexHsDoc :: P (GenLocated loc RdrName) -- ^ A precise identifier parser
         -> String                     -- ^ A docstring
         -> HsDoc RdrName
lexHsDoc parseIdentifier s = HsDoc (mkHsDocString s) (mapMaybe maybeDocIdentifier idxdPlausIds)
  where
    maybeDocIdentifier :: (Int, String, Int) -> Maybe (HsDocIdentifier RdrName)
    maybeDocIdentifier (ix0, pid, ix1) =
      HsDocIdentifier (HsDocIdentifierSpan ix0 ix1) (mkHsDocString pid) . (: [])
        <$> parseIdent parseIdentifier pid
    idxdPlausIds =
      either (error . show)
             id
             (P.runParser (identifiersWith (delimited plausibleIdentifierWithIndices))
                          () "" s)

identifiersWith :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
identifiersWith identifier =
    catMaybes <$> P.many (P.try (Just <$> identifier) <|> handleNewline <|> dropDelim <|> dropUntilDelim)
  where
    handleNewline = do
      p0 <- P.getPosition
      _ <- P.char '\n'
      P.setPosition (P.incSourceColumn p0 1)
      return Nothing
    dropUntilDelim = P.skipMany1 (P.satisfy (\c -> not (isDelim c) && c /= '\n')) $> Nothing
    dropDelim = identDelim $> Nothing

plausibleIdentifierWithIndices :: Stream s m Char => ParsecT s u m (Int, String, Int)
plausibleIdentifierWithIndices = liftA3 (,,) getColPos plausibleIdentifier getColPos
  where
    -- Instead of subtracting 1 from the sourceColumn
    -- we could use P.setPosition to start at 0.
    getColPos = (pred . P.sourceColumn) <$> P.getPosition

delimited :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
delimited = P.between identDelim identDelim

plausibleIdentifier :: Stream s m Char => ParsecT s u m String
plausibleIdentifier = do
    c <- P.satisfy isFirstIdentChar
    cs <- p
    return (c : cs)
  where
    p = do
      vs <- many (P.satisfy (\c -> isIdentChar c && c /= '\''))
      c <- P.lookAhead P.anyChar
      case c of
        '`' -> return vs
        '\'' -> P.try ((\x -> vs ++ "'" ++ x) <$> (P.char '\'' *> p)) <|> return vs
        _ -> fail "outofvalid"

identDelim :: Stream s m Char => ParsecT s u m Char
identDelim = P.satisfy isDelim

isDelim :: Char -> Bool
isDelim c = c == '\'' || c == '`'

isFirstIdentChar :: Char -> Bool
isFirstIdentChar c = (isAlpha c || c == '_' || isSymbol c || c == ':') && not (isDelim c)

isIdentChar :: Char -> Bool
isIdentChar c = not (isSpace c) && c /= '`'

-- adapted from haddock-api
parseIdent :: P (GenLocated loc RdrName) -> String -> Maybe RdrName
parseIdent parseIdentifier str0 =
  let pflags = ParserFlags EnumSet.empty EnumSet.empty (stringToUnitId "") 0
      buffer = stringToStringBuffer str0
      realSrcLc = mkRealSrcLoc (mkFastString "") 0 0
      pstate = mkPStatePure pflags buffer realSrcLc
  in case unP parseIdentifier pstate of
    POk _ name -> Just (unLoc name)
    _ -> Nothing
