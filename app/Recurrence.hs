module Recurrence where

data Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Eq, Show)

data Recurrence
  = EveryDay
  | EveryWeekday Weekday
  | NoRecurrence
  deriving (Eq, Show)