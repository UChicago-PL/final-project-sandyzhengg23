module Event where

import DateExpr
import TimeRange
import Recurrence

data Event = Event
  { eventTitle      :: String
  , eventDate       :: DateExpr
  , eventTime       :: TimeRange
  , eventLocation   :: Maybe String
  , eventRecurrence :: Recurrence
  }
  deriving (Eq, Show)