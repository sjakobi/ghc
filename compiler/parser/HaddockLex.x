{
-- TODO: Update Note [Warnings in code generated by Alex] in
-- compiler/parser/Lexer.x and the warnings prescribed in it.
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

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

import Data.Bits
import Data.Char
import Data.Int
import Data.Maybe
import Data.Word
}

-- The character sets marked "TODO" are mostly overly inclusive
-- and should be defined more precisely once alex has better
-- support for unicode character sets (see
-- https://github.com/simonmar/alex/issues/126).
$special   = [\(\)\,\;\[\]\`\{\}]
$asciisymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]
$asciidigit = 0-9
$digit = $asciidigit                             -- TODO
$asciilower = [a-z \_]
$asciiupper = A-Z
$asciialpha = [$asciilower $asciiupper]
$interesting = $printable # [$white $special]
$alpha = $interesting # [$digit $asciisymbol \'] -- TODO
$upper = $alpha # $asciilower                    -- TODO
$symbol = $interesting # [$digit $asciialpha \'] -- TODO
$idchar = [$alpha $digit \']
$delim = [\'\`]

@id = $alpha $idchar* | $symbol+
@modname = $upper $idchar*
@qualid = (@modname \.)* @id

:-
  $delim @qualid $delim { getIdentifier }
  [. \n] ;

{
-- NB: As long as we don't use a left-context we don't need to track the
-- previous input character.
data AlexInput = AlexInput
  { alexInput_position     :: !AlexPosn
  , alexInput_pendingBytes :: [Word8]
  , alexInput_string       :: String
  }

-- | Simple character offset.
type AlexPosn = Int

alexStartPos :: AlexPosn
alexStartPos = 0

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AlexInput p (b:bs) s    ) = Just (b, AlexInput p bs s)
alexGetByte (AlexInput p []     (c:s)) = let (b:bs) = utf8Encode c
                                         in Just (b, AlexInput (succ p) bs s)
alexGetByte (AlexInput p []     ""   ) = Nothing

-- | Encode a Haskell 'Char' to a list of 'Word8' values, in UTF8 format.

-- Adapted from Alex.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]
   | oc <= 0x7ff      = [ 0xc0 + (oc `unsafeShiftR` 6)
                        , 0x80 + oc .&. 0x3f
                        ]
   | oc <= 0xffff     = [ 0xe0 + (oc `unsafeShiftR` 12)
                        , 0x80 + ((oc `unsafeShiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `unsafeShiftR` 18)
                        , 0x80 + ((oc `unsafeShiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `unsafeShiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = error "Left-context not supported"

alexScanTokens :: String -> [(Int, String, Int)]
alexScanTokens str0 = go (AlexInput alexStartPos [] str0)
  where go inp@(AlexInput pos _ str) =
          case alexScan inp 0 of
                AlexSkip  inp' _ln            -> go inp'
                AlexToken inp' len act        -> act pos len str : go inp'
                AlexEOF                       -> []
                AlexError (AlexInput off _ _) ->
                  error $ "lexical error at offset " ++ show off

--------------------------------------------------------------------------------

-- | Extract identifier from Alex state.
getIdentifier :: AlexPosn
              -> Int
                 -- ^ Token length
              -> String
                 -- ^ The remaining input starting at the current token
              -> (Int, String, Int)
getIdentifier off0 len0 s0 =
    (off1, s1, off1 + len1)
  where
    off1 = succ off0
    len1 = len0 - 2
    s1 = take len1 (tail s0)

-- | Lex identifiers from a docstring.
lexHsDoc :: P RdrName      -- ^ A precise identifier parser
         -> String         -- ^ A docstring
         -> HsDoc RdrName
lexHsDoc identParser s =
    HsDoc (mkHsDocString s) (mapMaybe maybeDocIdentifier plausibleIdents)
  where
    maybeDocIdentifier :: (Int, String, Int) -> Maybe (HsDocIdentifier RdrName)
    maybeDocIdentifier (ix0, pid, ix1) =
      HsDocIdentifier (HsDocIdentifierSpan ix0 ix1) . (: [])
        <$> validateIdentWith identParser pid

    plausibleIdents :: [(Int, String, Int)]
    plausibleIdents = alexScanTokens s

validateIdentWith :: P RdrName -> String -> Maybe RdrName
validateIdentWith identParser str0 =
  let pflags = ParserFlags EnumSet.empty EnumSet.empty (stringToUnitId "") 0
      buffer = stringToStringBuffer str0
      realSrcLc = mkRealSrcLoc (mkFastString "") 0 0
      pstate = mkPStatePure pflags buffer realSrcLc
  in case unP identParser pstate of
    POk _ name -> Just name
    _ -> Nothing
}
