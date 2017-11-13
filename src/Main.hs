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
               "ordered by number of comments decreasing:\n"
  printEntries tLongsByComments

  putStrLn $ "\nSubset of " ++ (show.length$ tShortsByPoints) ++
               " entries with 5 or fewer words in title, \n" ++
               "ordered by number of points decreasing:\n"
  printEntries tShortsByPoints

    where
      titleG5 = (>5) . length . words . title
      (tLongs, tShorts) = partition titleG5 es
      tLongsByComments =  reverse . sortBy (compare `on` commented) $ tLongs
      tShortsByPoints =  reverse . sortBy (compare `on` points) $ tShorts


printEntries :: [EntryYC] -> IO ()
printEntries = putStrLn . unlines . map show
