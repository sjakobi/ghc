-- This function currently doesn't simplify using the
-- combine-identical-alternatives optimisation

module T14684 where

import GHC.List as L

data Foo = Foo1 | Foo2 | Foo3 !Int | Foo4 | Foo5

fun1 :: Foo -> Int
{-# NOINLINE fun1 #-}
fun1 x = case x of
               Foo1 -> 0
               Foo2 -> 1
               Foo3 {} -> 1
               Foo4 -> 2
               Foo5 -> 2
