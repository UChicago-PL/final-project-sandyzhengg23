module DateExpr where

data DateExpr
  = Today
  | Tomorrow
  | OnDate String
  | UnknownDate
  deriving (Eq, Show)