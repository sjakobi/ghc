{-# language TemplateHaskell #-}
module Curries where

import Control.Monad
import Language.Haskell.TH

genCurries :: Int -> Q [Dec]
genCurries n = forM [1..n] mkCurryDec
  where mkCurryDec ith = do
           cury <- curryN ith
           let name = mkName $ "curry" ++ show ith
           return $ FunD name [Clause [] (NormalB cury) []]

curryN :: Int -> Q Exp
curryN n = do
  f  <- newName "f"
  xs <- replicateM n (newName "x")
  let args = map VarP (f:xs)
      ntup = TupE (map VarE xs)
  return $ LamE args (AppE (VarE f) ntup)
