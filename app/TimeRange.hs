module TimeRange where

data TimeRange
  = SingleTime String
  | TimeSpan String String
  | UnknownTime
  deriving (Eq, Show)