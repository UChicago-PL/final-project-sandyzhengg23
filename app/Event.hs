module Event where

import DateExpr
import TimeRange

data Event = Event
  { eventTitle    :: String
  , eventDate     :: DateExpr
  , eventTime     :: TimeRange
  , eventLocation :: Maybe String
  }
  deriving (Eq, Show)