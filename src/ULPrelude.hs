module ULPrelude where

import Data.Map (Map)
import qualified Data.Map as M

import IR

typ2val :: Type -> TValue
typ2val typ = TValue (VType typ) TType

typ :: TValue
typ = typ2val TType

boolt :: Type
boolt = TBool

bool :: TValue
bool = typ2val TBool

int :: TValue
int = typ2val $ TInt $ IntType 32 True False

true :: TValue
true = TValue (VBool True) boolt

false :: TValue
false = TValue (VBool False) boolt

prelude :: TProgram
prelude =
  M.fromList
    [ ("Type", typ)
    , ("Bool", bool)
    , ("Int", int)
    , ("True", true)
    , ("False", false)
    ]
