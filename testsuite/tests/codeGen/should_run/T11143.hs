{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import Control.Monad
import GHC.Base

data BA = BA ByteArray#

-- Checks that indexByteInt16Array# works
main :: IO ()
main = do BA ba <- IO $ \s0 ->
                        case newByteArray# 5# s0 of
                        (# !s1, !mba #) ->
                            case setByteArray# mba 0# 5# 0# s1 of
                            !s2 ->
                                case setByteArray# mba 1# 3# 1# s2 of
                                !s3 ->
                                    case unsafeFreezeByteArray# mba s3 of
                                    (# s4, ba #) -> (# s4, BA ba #)
          let f (I# b_ix) (I# i16_ix) = print (I# (indexByteInt16Array# ba b_ix i16_ix))
          forM_ [0..3] $ \i -> f i 0
          putStrLn "---"
          forM_ [0..1] $ \i -> f 0 i
          putStrLn "---"
          f 1 1


