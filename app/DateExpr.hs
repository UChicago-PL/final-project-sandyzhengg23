module DateExpr where

data DateExpr
  = Tomorrow
  | OnDate String
  | UnknownDate
  deriving (Eq, Show)