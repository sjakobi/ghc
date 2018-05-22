module ConLike where
import GhcPrelude
import {-# SOURCE #-} DataCon (DataCon)
import {-# SOURCE #-} PatSyn (PatSyn)
import Name ( Name )

data ConLike = RealDataCon DataCon
             | PatSynCon PatSyn

conLikeName :: ConLike -> Name

instance Show ConLike
