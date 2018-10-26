{-# language MagicHash #-}

-- | 'Char#' '-#' '<=##' '#' @'succ#'0#@ 'Bool'
module DocsWithMagicHash where

import GHC.Prim

succ# :: Int# -> Int#
succ# = (+# 1#)
