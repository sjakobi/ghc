-- This function currently doesn't simplify using the
-- combine-identical-alternatives optimisation.
-- After the fix, Foo3 {}, Foo5 and Foo6 should be combined
-- to a single DEFAULT alternative.


module T14684 where

import GHC.List as L

data Foo = Foo1 | Foo2 | Foo3 !Int | Foo4 | Foo5 | Foo6

fun1 :: Foo -> Int
{-# NOINLINE fun1 #-}
fun1 x = case x of
               Foo1 -> 0
               Foo2 -> 1
               Foo3 {} -> 2
               Foo4 -> 1
               Foo5 -> 2
               Foo6 -> 2
