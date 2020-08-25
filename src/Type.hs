import Data.List as L

data IntType =
  IntType
    { size :: Int
    , signed :: Bool
    , wrapping :: Bool
    }
  deriving (Eq, Show)

data Type
  = Bool
  | Int IntType
  | Tuple [Type]
  | Choice [Type]
  | Sum [(String, Type)]
  | Struct [(String, Type)]
  | LinArrow Type Type
  | UnrArrow Type Type
