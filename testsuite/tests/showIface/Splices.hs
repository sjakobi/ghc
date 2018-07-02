{-# language TemplateHaskell #-}
module Splices where

import Curries

$(genCurries 3)
