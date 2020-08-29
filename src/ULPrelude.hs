module ULPrelude where

import Data.Map (Map)
import qualified Data.Map as M

import IR

comptimetyp :: TypeBase -> Type
comptimetyp = Type True

typ2val :: Type -> TValue
typ2val typ = TValue (VType typ) $ comptimetyp TType

typ :: TValue
typ = typ2val $ comptimetyp TType

boolt :: Type
boolt = comptimetyp TBool

bool :: TValue
bool = typ2val boolt

int :: TValue
int = typ2val $ comptimetyp $ TInt $ IntType 32 True False

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
