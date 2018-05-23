{-| `elem`, 'print',
`Unknown',
'<>', ':=:', 'Bool'
-}
module DocsInHiFile where

-- | '()', 'elem'.
elem :: ()
elem = ()

-- | A datatype.
data D
  = D0 -- ^ A constructor for 'D'. '
  | D1 -- ^ Another constructor

add :: Int -- ^ First summand for 'add'
    -> Int -- ^ Second summand
    -> Int -- ^ Sum
add a b = a + b
