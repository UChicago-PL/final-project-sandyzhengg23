module Parse (parseEvent) where

import Event
import DateExpr
import TimeRange

parseEvent :: String -> Event
parseEvent input =
  let (noLoc, locPart) = extractLocation input
      (noDate, datePart) = extractTomorrow (trim noLoc)
      (titlePart, timePart) = extractTime (trim noDate)
  in Event
      { eventTitle = trim titlePart
      , eventDate = datePart
      , eventTime = timePart
      , eventLocation = locPart
      }

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
  where
    isSpace c = c == ' ' || c == '\t'
    dropWhileEnd p = reverse . dropWhile p . reverse

extractTomorrow :: String -> (String, DateExpr)
extractTomorrow s =
  if "tomorrow" `elem` words s
    then (unwords (filter (/= "tomorrow") (words s)), Tomorrow)
    else (s, UnknownDate)

extractLocation :: String -> (String, Maybe String)
extractLocation s =
  case break (== '@') s of
    (before, '@':after) -> (trim before, Just (trim after))
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
    (_, []) -> Nothing
    (a, _:b) -> Just (a, b)


extractTime :: String -> (String, TimeRange)
extractTime s =
  case findTimeSpan (words s) of
    Just (before, startT, endT, after) ->
      (unwords (before ++ after), TimeSpan startT endT)
    Nothing ->
      case findSingleTime (words s) of
        Just (before, t, after) ->
          (unwords (before ++ after), SingleTime t)
        Nothing ->
          (s, UnknownTime)

findTimeSpan :: [String] -> Maybe ([String], String, String, [String])
findTimeSpan ws =
  case ws of
    [] -> Nothing
    (x:y:rest)
      | looksLikeRange x && (y == "am" || y == "pm") ->
          let (startRaw, endRaw) = splitRange x
              startT = startRaw ++ y
              endT   = endRaw ++ y
          in Just ([], startT, endT, rest)
      | looksLikeRangeWithMeridiem x ->
          let (rangePart, mer) = splitMeridiem x
              (startRaw, endRaw) = splitRange rangePart
              startT = startRaw ++ mer
              endT   = endRaw ++ mer
          in Just ([], startT, endT, y:rest)
      | otherwise ->
          prepend x (findTimeSpan (y:rest))
    (x:rest) ->
      prepend x (findTimeSpan rest)
  where
    prepend a mb =
      case mb of
        Nothing -> Nothing
        Just (before, s1, s2, after) -> Just (a:before, s1, s2, after)

findSingleTime :: [String] -> Maybe ([String], String, [String])
findSingleTime ws =
  case ws of
    [] -> Nothing
    (x:rest)
      | looksLikeSingleTime x -> Just ([], x, rest)
      | otherwise -> prepend x (findSingleTime rest)
  where
    prepend a mb =
      case mb of
        Nothing -> Nothing
        Just (before, t, after) -> Just (a:before, t, after)

looksLikeRange :: String -> Bool
looksLikeRange t =
  case break (== '-') t of
    (l, '-':r) -> allDigits l && allDigits r
    _          -> False

splitRange :: String -> (String, String)
splitRange t =
  case break (== '-') t of
    (l, '-':r) -> (l, r)
    _          -> (t, t)

looksLikeRangeWithMeridiem :: String -> Bool
looksLikeRangeWithMeridiem t =
  case reverse t of
    ('m':'a':restRev) -> looksLikeRange (reverse restRev)
    ('m':'p':restRev) -> looksLikeRange (reverse restRev)
    _                 -> False

splitMeridiem :: String -> (String, String)
splitMeridiem t =
  let rt = reverse t
  in case rt of
      ('m':'a':restRev) -> (reverse restRev, "am")
      ('m':'p':restRev) -> (reverse restRev, "pm")
      _                 -> (t, "")

looksLikeSingleTime :: String -> Bool
looksLikeSingleTime t =
  case reverse t of
    ('m':'a':restRev) -> hasDigits (reverse restRev)
    ('m':'p':restRev) -> hasDigits (reverse restRev)
    _ -> False

hasDigits :: String -> Bool
hasDigits = any (`elem` "0123456789")

allDigits :: String -> Bool
allDigits [] = False
allDigits cs = all (`elem` "0123456789") cs