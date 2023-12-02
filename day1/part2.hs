{-# OPTIONS_GHC -Wno-x-partial #-}
module Main where

import Data.Char
import Data.List
import Data.Maybe

asDigit :: String -> Maybe Int
asDigit str
    | "1" `isPrefixOf` str || "one"   `isPrefixOf` str  = Just 1
    | "2" `isPrefixOf` str || "two"   `isPrefixOf` str  = Just 2
    | "3" `isPrefixOf` str || "three" `isPrefixOf` str  = Just 3
    | "4" `isPrefixOf` str || "four"  `isPrefixOf` str  = Just 4
    | "5" `isPrefixOf` str || "five"  `isPrefixOf` str  = Just 5
    | "6" `isPrefixOf` str || "six"   `isPrefixOf` str  = Just 6
    | "7" `isPrefixOf` str || "seven" `isPrefixOf` str  = Just 7
    | "8" `isPrefixOf` str || "eight" `isPrefixOf` str  = Just 8
    | "9" `isPrefixOf` str || "nine"  `isPrefixOf` str  = Just 9
    | otherwise = Nothing

main :: IO ()
main = do
    lines <- lines <$> readFile "input.txt"
    print $ sum $ map (\line -> let digits = mapMaybe asDigit (tails line) in 10 * head digits + last digits) lines
