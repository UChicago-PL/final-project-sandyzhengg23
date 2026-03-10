module Parse (parseEvent) where

import DateExpr
import Event
import Recurrence
import TimeRange

parseEvent :: String -> Event
parseEvent input =
  let (noLoc, locPart) = extractLocation input
      (noRec, recPart) = extractRecurrence (trim noLoc)
      (noDate, datePart) = extractDateExpr (trim noRec)
      (titlePart, timePart) = extractTime (trim noDate)
  in Event
      { eventTitle = trim titlePart
      , eventDate = datePart
      , eventTime = timePart
      , eventLocation = locPart
      , eventRecurrence = recPart
      }

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
  where
    isSpace c = c == ' ' || c == '\t'
    dropWhileEnd p = reverse . dropWhile p . reverse

extractRecurrence :: String -> (String, Recurrence)
extractRecurrence s =
  case map lowerString (words s) of
    ("every":"day":rest) ->
      (removeLeadingEveryThing s, EveryDay)
    ("every":"monday":rest) ->
      (removeLeadingEveryThing s, EveryWeekday Monday)
    ("every":"tuesday":rest) ->
      (removeLeadingEveryThing s, EveryWeekday Tuesday)
    ("every":"wednesday":rest) ->
      (removeLeadingEveryThing s, EveryWeekday Wednesday)
    ("every":"thursday":rest) ->
      (removeLeadingEveryThing s, EveryWeekday Thursday)
    ("every":"friday":rest) ->
      (removeLeadingEveryThing s, EveryWeekday Friday)
    ("every":"saturday":rest) ->
      (removeLeadingEveryThing s, EveryWeekday Saturday)
    ("every":"sunday":rest) ->
      (removeLeadingEveryThing s, EveryWeekday Sunday)
    _ ->
      (s, NoRecurrence)

removeLeadingEveryThing :: String -> String
removeLeadingEveryThing s =
  case words s of
    (_ : _ : rest) -> unwords rest
    _              -> s

lowerString :: String -> String
lowerString = map toLowerSimple

toLowerSimple :: Char -> Char
toLowerSimple c
  | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
  | otherwise = c

extractDateExpr :: String -> (String, DateExpr)
extractDateExpr s
  | "tomorrow" `elem` ws = (removeWord "tomorrow" ws, Tomorrow)
  | "today" `elem` ws    = (removeWord "today" ws, Today)
  | otherwise =
      case extractExplicitDate ws of
        Just (remainingWords, d) -> (unwords remainingWords, OnDate d)
        Nothing                  -> (s, UnknownDate)
  where
    ws = words s

removeWord :: String -> [String] -> String
removeWord w = unwords . filter (/= w)

extractExplicitDate :: [String] -> Maybe ([String], String)
extractExplicitDate [] = Nothing
extractExplicitDate (x:xs)
  | looksLikeISODate x = Just (xs, x)
  | otherwise =
      case extractExplicitDate xs of
        Just (rest, d) -> Just (x:rest, d)
        Nothing        -> Nothing

looksLikeISODate :: String -> Bool
looksLikeISODate s =
  case splitOnDash3 s of
    Just (y, m, d) ->
      length y == 4 && length m == 2 && length d == 2
      && allDigits y && allDigits m && allDigits d
    Nothing -> False

splitOnDash3 :: String -> Maybe (String, String, String)
splitOnDash3 s =
  case break (== '-') s of
    (a, '-':rest1) ->
      case break (== '-') rest1 of
        (b, '-':rest2) -> Just (a, b, rest2)
        _              -> Nothing
    _ -> Nothing

extractLocation :: String -> (String, Maybe String)
extractLocation s =
  case break (== '@') s of
    (before, '@' : after) ->
      (trim before, Just (trim after))
    _ ->
      case breakOnWord "at" (words s) of
        Just (beforeWords, afterWords) ->
          (unwords beforeWords, Just (unwords afterWords))
        Nothing ->
          (s, Nothing)

breakOnWord :: String -> [String] -> Maybe ([String], [String])
breakOnWord _ [] = Nothing
breakOnWord w xs =
  case break (== w) xs of
    (_, [])   -> Nothing
    (a, _:b)  -> Just (a, b)

extractTime :: String -> (String, TimeRange)
extractTime s =
  let ws = words s
  in case pickSpan ws of
      Just (ws', startT, endT) -> (unwords ws', TimeSpan startT endT)
      Nothing ->
        case pickSingle ws of
          Just (ws', t) -> (unwords ws', SingleTime t)
          Nothing       -> (s, UnknownTime)

pickSpan :: [String] -> Maybe ([String], String, String)
pickSpan [] = Nothing
pickSpan [x] =
  case splitRangeWithMer x of
    Just (a, b, mer) -> Just ([], a ++ mer, b ++ mer)
    Nothing          -> Nothing
pickSpan (x:y:rest)
  | isRange x && isMer y =
      let (a, b) = splitOnDash x
      in Just (rest, a ++ y, b ++ y)
  | otherwise =
      case splitRangeWithMer x of
        Just (a, b, mer) -> Just (y:rest, a ++ mer, b ++ mer)
        Nothing ->
          case pickSpan (y:rest) of
            Just (ws', s1, s2) -> Just (x:ws', s1, s2)
            Nothing            -> Nothing

pickSingle :: [String] -> Maybe ([String], String)
pickSingle [] = Nothing
pickSingle [x]
  | isSingleTime x = Just ([], x)
  | otherwise      = Nothing
pickSingle (x:y:rest)
  | allDigits x && isMer y = Just (rest, x ++ y)
  | isSingleTime x         = Just (y:rest, x)
  | otherwise =
      case pickSingle (y:rest) of
        Just (ws', t) -> Just (x:ws', t)
        Nothing       -> Nothing

isMer :: String -> Bool
isMer w = w == "am" || w == "pm"

isRange :: String -> Bool
isRange t =
  case break (== '-') t of
    (l, '-':r) -> allDigits l && allDigits r
    _          -> False

splitOnDash :: String -> (String, String)
splitOnDash t =
  case break (== '-') t of
    (l, '-':r) -> (l, r)
    _          -> (t, t)

splitRangeWithMer :: String -> Maybe (String, String, String)
splitRangeWithMer t =
  if endsWith "am" t
    then mk "am"
    else if endsWith "pm" t
      then mk "pm"
      else Nothing
  where
    mk mer =
      let rangePart = take (length t - 2) t
      in if isRange rangePart
           then let (a, b) = splitOnDash rangePart
                in Just (a, b, mer)
           else Nothing

isSingleTime :: String -> Bool
isSingleTime t =
  case stripMeridiem t of
    Just numPart -> looksLikeClock numPart
    Nothing      -> False

stripMeridiem :: String -> Maybe String
stripMeridiem t
  | endsWith "am" t = Just (take (length t - 2) t)
  | endsWith "pm" t = Just (take (length t - 2) t)
  | otherwise       = Nothing

looksLikeClock :: String -> Bool
looksLikeClock s =
  case break (== ':') s of
    (h, ':' : m) -> allDigits h && allDigits m
    (h, _)       -> allDigits h

endsWith :: String -> String -> Bool
endsWith suf s = reverse suf == take (length suf) (reverse s)

allDigits :: String -> Bool
allDigits [] = False
allDigits cs = all (`elem` "0123456789") cs