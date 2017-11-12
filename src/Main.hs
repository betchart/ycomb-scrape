{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.HTML.Scalpel
import Control.Applicative
import Control.Monad

data Entry = Entry { rank :: Int
                   , title :: String
                   , points :: Int
                   , commented :: Int
                   } deriving (Show)

main :: IO ()
main = do
  stuff <- scrapeURL "https://news.ycombinator.com" entriesYComb
  maybe printError printHeadlines stuff

printError :: IO()
printError = putStrLn "ERROR: Could not scrape the URL!"

printHeadlines :: [Entry] -> IO()
printHeadlines hs = putStrLn $ unlines $ map show hs

entriesYComb :: Scraper String [Entry]
entriesYComb = do hdlns <- headlines
                  sbtxs <- subtexts
                  return [Entry r t s c | ((r,t),(s, c)) <- zip hdlns sbtxs]

headlines :: Scraper String [(Int, String)]
headlines = chroots ("tr" @: [hasClass "athing"]) $ do
              sRank <- text $ "span" @: [hasClass "rank"]
              sTitle <- text $ "a"  @: [hasClass "storylink"]
              return (nRank sRank, sTitle)

subtexts :: Scraper String [(Int, Int)]
subtexts = chroots ("td" @: [hasClass "subtext"]) $ do
             sPoints <- text $ "span" @: [hasClass "score"]
             links <- texts $ "a" -- comments are the fourth link
             return ( nPoints sPoints, nComments $ last links)


nRank :: String       -- ^ Has the form "NN."
      -> Int          -- ^ NN parsed as int
nRank s = read n :: Int
    where n = reverse . drop 1 $ reverse s
                    
nComments :: String     -- ^ Has the form "NN\160comments" or "discuss"
          -> Int        -- ^ NN parsed as int
nComments "discuss" = 0
nComments s = read n :: Int
    where n = head $ words s

nPoints :: String      -- ^ Has the form "NN points"
        -> Int         -- ^ NN parsed as int
nPoints s = read n :: Int
    where n = head $ words s
