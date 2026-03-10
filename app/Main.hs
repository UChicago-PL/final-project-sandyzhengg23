module Main where

import Data.Time
import ICS (renderICS)
import Parse (parseEvent)

main :: IO ()
main = do
  today <- utctDay <$> getCurrentTime
  putStrLn "Enter events, one per line. Press Enter on a blank line to finish:"
  linesOfInput <- getEventLines

  let events = map parseEvent linesOfInput

  putStrLn "\nParsed events:"
  mapM_ print events

  writeFile "calendar.ics" (renderICS today events)
  putStrLn "\nGenerated calendar.ics"

getEventLines :: IO [String]
getEventLines = do
  line <- getLine
  if null line
    then return []
    else do
      rest <- getEventLines
      return (line : rest)