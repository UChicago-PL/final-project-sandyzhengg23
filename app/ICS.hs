module ICS (renderICS) where

import Event

renderICS :: Event -> String
renderICS event =
  unlines
    [ "BEGIN:VCALENDAR"
    , "BEGIN:VEVENT"
    , "SUMMARY:" ++ eventTitle event
    , "END:VEVENT"
    , "END:VCALENDAR"
    ]