module Error where

data Error =
  DefsNotFound [String]

instance Show Error where
  show (DefsNotFound defs) = "Undefined references to " ++ show defs
