module MkId where
import GhcPrelude
import Name( Name )
import Var( Id )
import Class( Class )
import {-# SOURCE #-} DataCon( DataCon )
import {-# SOURCE #-} PrimOp( PrimOp )

data DataConBoxer

mkDataConWorkId :: Name -> DataCon -> Id
mkDictSelId     :: Name -> Class   -> Id

mkPrimOpId      :: PrimOp -> Id

magicDictId :: Id

instance Show DataConBoxer
