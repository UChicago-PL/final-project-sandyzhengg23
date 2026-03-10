module ICS (renderICS) where

import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Time (Day, addDays, defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.Calendar.WeekDate (toWeekDate)
import DateExpr
import Event
import Recurrence
import Text.Printf (printf)
import TimeRange

renderICS :: Day -> [Event] -> String
renderICS today events =
  joinICS $
    [ "BEGIN:VCALENDAR"
    , "VERSION:2.0"
    , "PRODID:-//Sandy Zheng//Natural Language Task Calendar//EN"
    , "CALSCALE:GREGORIAN"
    , "METHOD:PUBLISH"
    ]
    ++ concatMap (uncurry (renderEvent today)) (zip [1 :: Int ..] events)
    ++ [ "END:VCALENDAR" ]

renderEvent :: Day -> Int -> Event -> [String]
renderEvent today idx event =
  [ "BEGIN:VEVENT"
  -- Changed prefix to v2-event to bust Google Calendar's broken cache
  , "UID:v2-event-" ++ show idx ++ "@final-project" 
  , "DTSTAMP:" ++ formatDayICS today ++ "T000000Z"
  , "SUMMARY:" ++ escapeText (eventTitle event)
  ]
  ++ locationField
  ++ [ "DTSTART;TZID=America/Chicago:" ++ startStamp
     , "DTEND;TZID=America/Chicago:" ++ endStamp
     ]
  ++ recurrenceField (eventRecurrence event)
  ++ descriptionField
  ++ [ "END:VEVENT" ]
  where
    dayForEvent = resolveEventDay today event
    ((startH, startM), (endH, endM)) = resolveTime (eventTime event)

    startStamp = formatDateTimeICS dayForEvent startH startM
    endStamp   = formatDateTimeICS dayForEvent endH endM

    locationField =
      case eventLocation event of
        Just loc -> ["LOCATION:" ++ escapeText loc]
        Nothing  -> []

    descriptionParts =
      catMaybes
        [ case eventDate event of
            Today       -> Just "Date = today"
            Tomorrow    -> Just "Date = tomorrow"
            OnDate d    -> Just ("Date = " ++ d)
            UnknownDate -> Nothing
        , case eventTime event of
            SingleTime t -> Just ("Time = " ++ t)
            TimeSpan s e -> Just ("Time = " ++ s ++ "-" ++ e)
            UnknownTime  -> Nothing
        ]

    descriptionField =
      if null descriptionParts
        then []
        else ["DESCRIPTION:" ++ escapeText (intercalate "; " descriptionParts)]

recurrenceField :: Recurrence -> [String]
recurrenceField rec =
  case rec of
    EveryDay ->
      ["RRULE:FREQ=DAILY"]
    EveryWeekday Monday ->
      ["RRULE:FREQ=WEEKLY;BYDAY=MO"]
    EveryWeekday Tuesday ->
      ["RRULE:FREQ=WEEKLY;BYDAY=TU"]
    EveryWeekday Wednesday ->
      ["RRULE:FREQ=WEEKLY;BYDAY=WE"]
    EveryWeekday Thursday ->
      ["RRULE:FREQ=WEEKLY;BYDAY=TH"]
    EveryWeekday Friday ->
      ["RRULE:FREQ=WEEKLY;BYDAY=FR"]
    EveryWeekday Saturday ->
      ["RRULE:FREQ=WEEKLY;BYDAY=SA"]
    EveryWeekday Sunday ->
      ["RRULE:FREQ=WEEKLY;BYDAY=SU"]
    NoRecurrence ->
      []

resolveEventDay :: Day -> Event -> Day
resolveEventDay today event =
  case eventRecurrence event of
    EveryDay ->
      resolveDate today (eventDate event)
    EveryWeekday Monday ->
      nextWeekday 1 baseDay
    EveryWeekday Tuesday ->
      nextWeekday 2 baseDay
    EveryWeekday Wednesday ->
      nextWeekday 3 baseDay
    EveryWeekday Thursday ->
      nextWeekday 4 baseDay
    EveryWeekday Friday ->
      nextWeekday 5 baseDay
    EveryWeekday Saturday ->
      nextWeekday 6 baseDay
    EveryWeekday Sunday ->
      nextWeekday 7 baseDay
    NoRecurrence ->
      baseDay
  where
    baseDay = resolveDate today (eventDate event)

resolveDate :: Day -> DateExpr -> Day
resolveDate today expr =
  case expr of
    Today       -> today
    Tomorrow    -> addDays 1 today
    OnDate s    ->
      case parseTimeM True defaultTimeLocale "%Y-%m-%d" s of
        Just d  -> d
        Nothing -> today
    UnknownDate -> today

weekdayNumber :: Day -> Int
weekdayNumber day =
  let (_, _, w) = toWeekDate day
  in w

nextWeekday :: Int -> Day -> Day
nextWeekday target day =
  let current = weekdayNumber day
      diff =
        if target > current
          then target - current
          else if target == current
            then 0 -- Changed from 7 to 0 so it doesn't skip today!
            else 7 - (current - target)
  in addDays (fromIntegral diff) day

resolveTime :: TimeRange -> ((Int, Int), (Int, Int))
resolveTime tr =
  case tr of
    SingleTime t ->
      case parseClock t of
        Just (h, m) -> ((h, m), addMinutes 60 (h, m))
        Nothing     -> ((9, 0), (10, 0))
    TimeSpan s e ->
      case (parseClock s, parseClock e) of
        (Just a, Just b) -> (a, b)
        _                -> ((9, 0), (10, 0))
    UnknownTime ->
      ((9, 0), (10, 0))

parseClock :: String -> Maybe (Int, Int)
parseClock raw =
  case meridiemPart of
    "am" -> Just (to24 False hour, minute)
    "pm" -> Just (to24 True hour, minute)
    _    -> Nothing
  where
    lower = map toLowerSimple raw
    meridiemPart =
      if endsWith "am" lower then "am"
      else if endsWith "pm" lower then "pm"
      else ""

    -- Strips out spaces so "3 pm" and "3pm" both parse correctly
    numericPart = filter (/= ' ') (take (length lower - 2) lower)

    (hourStr, minute) =
      case break (== ':') numericPart of
        (h, ':' : m) -> (h, readIntSafe m)
        (h, _)       -> (h, 0)

    hour = readIntSafe hourStr

to24 :: Bool -> Int -> Int
to24 isPM h
  | isPM && h < 12 = h + 12
  | not isPM && h == 12 = 0
  | otherwise = h

addMinutes :: Int -> (Int, Int) -> (Int, Int)
addMinutes mins (h, m) =
  let total = h * 60 + m + mins
  in ((total `div` 60) `mod` 24, total `mod` 60)

formatDayICS :: Day -> String
formatDayICS day =
  formatTime defaultTimeLocale "%Y%m%d" day

formatDateTimeICS :: Day -> Int -> Int -> String
formatDateTimeICS day h m =
  -- Removed the Z so it doesn't default to UTC
  formatDayICS day ++ "T" ++ printf "%02d%02d00" h m

readIntSafe :: String -> Int
readIntSafe s =
  case reads s of
    [(n, "")] -> n
    _         -> 0

endsWith :: String -> String -> Bool
endsWith suf s = reverse suf == take (length suf) (reverse s)

toLowerSimple :: Char -> Char
toLowerSimple c
  | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
  | otherwise = c

escapeText :: String -> String
escapeText [] = []
escapeText (c:cs)
  | c == '\\' = '\\' : '\\' : escapeText cs
  | c == ';'  = '\\' : ';'  : escapeText cs
  | c == ','  = '\\' : ','  : escapeText cs
  | c == '\n' = '\\' : 'n'  : escapeText cs
  | otherwise = c : escapeText cs

joinICS :: [String] -> String
joinICS [] = ""
joinICS xs = intercalate "\r\n" xs ++ "\r\n"