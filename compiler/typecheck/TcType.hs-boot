module TcType where
import GhcPrelude
import Outputable( SDoc )

data MetaDetails

data TcTyVarDetails
pprTcTyVarDetails :: TcTyVarDetails -> SDoc
vanillaSkolemTv :: TcTyVarDetails

instance Show TcTyVarDetails
