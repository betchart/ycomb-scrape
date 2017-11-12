module Main where

import Text.HTML.Scalpel
import Control.Applicative
import Control.Monad

import ScrapeNewsYC

main :: IO ()
main = do
  es <- scrapeURL "https://news.ycombinator.com" entriesYC
  maybe printError
        printEntries es

printError :: IO()
printError = putStrLn "ERROR: Could not scrape the URL!"

printEntries = putStrLn . unlines . map show
