module Main where

import Text.HTML.Scalpel (scrapeURL)
import Data.List (partition, sortBy)
import Data.Function (on)

import ScrapeNewsYC

url = "https://news.ycombinator.com"

main :: IO ()
main = do
  es <- scrapeURL url entriesYC
  maybe printError
        processEntries es

printError :: IO ()
printError = putStrLn "ERROR: Could not scrape the URL!"

processEntries:: [EntryYC] -> IO()
processEntries es = do

  putStrLn $ "\nAll " ++ (show.length$ es) ++
               " entries scraped from " ++ url ++ "\n"
  printEntries es

  putStrLn $ "\nSubset of " ++ (show.length$ tLongsByComments) ++
               " entries with more than 5 words in title,\n" ++
               "ordered by number of comments:\n"
  printEntries tLongsByComments

  putStrLn $ "\nSubset of " ++ (show.length$ tShortsByPoints) ++
               " entries with 5 or fewer words in title, \n" ++
               "ordered by number of points:\n"
  printEntries tShortsByPoints

    where (tLongsByComments, tShortsByPoints) = requestedFilters es

-- |We want it to be able to perform a couple of filtering operations:
--  * Filter all previous entries with more than five words in the title ordered by amount of comments first.
--  * Filter all previous entries with less than or equal to five words in the title ordered by points.
requestedFilters :: [EntryYC] -> ([EntryYC],[EntryYC])
requestedFilters es = (tLongsByComments, tShortsByPoints)
    where
      titleG5 = (>5) . length . words . title
      (tLongs, tShorts) = partition titleG5 es
      tLongsByComments =  sortBy (compare `on` commented) tLongs
      tShortsByPoints =  sortBy (compare `on` points) tShorts

printEntries :: [EntryYC] -> IO ()
printEntries = putStrLn . unlines . map show
