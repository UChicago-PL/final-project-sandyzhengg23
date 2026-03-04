module Main where

import Parse (parseEvent)

main :: IO ()
main = do
    putStrLn "Enter an event: "
    line <- getLine
    let event = parseEvent line
    print event