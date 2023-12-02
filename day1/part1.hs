{-# OPTIONS_GHC -Wno-x-partial #-}
module Main where

import Data.Char

main :: IO ()
main = do
    readFile "input.txt" >>= print . sum . map (\line -> let digits = map (read . pure) $ filter isDigit line in 10 * head digits + last digits) . lines