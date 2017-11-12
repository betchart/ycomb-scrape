{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.HTML.Scalpel
import Control.Applicative
import Control.Monad

data EntryYC = EntryYC { rank :: Int
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

printHeadlines :: [EntryYC] -> IO()
printHeadlines hs = putStrLn $ unlines $ map show hs

entriesYComb :: Scraper String [EntryYC]
entriesYComb = do hdlns <- headlinesYC
                  sbtxs <- subtextsYC
                  return $ zipEntriesYC hdlns sbtxs

zipEntriesYC :: [(Int, EntryYC)]  -- ^ Complete list of EntryYC
             -> [(Int, EntryYC)]  -- ^ Partial list of EntryYC with update values
             -> [EntryYC]
zipEntriesYC [] _ = []
zipEntriesYC xs [] = map snd xs
zipEntriesYC (x:xs) (y:ys) =
    case ix==iy of
      True -> ez:(zipEntriesYC xs ys)
      False -> ex:(zipEntriesYC xs (y:ys))
    where ez = EntryYC (rank ex) (title ex) (points ey) (commented ey)
          ex = snd x
          ey = snd y
          ix = fst x
          iy = fst y

headlinesYC :: Scraper String [(Int, EntryYC)]
headlinesYC = chroots ("tr" @: [hasClass "athing"]) $ do
                idval <- attr "id" $ "tr" @: [hasClass "athing"]
                sRank <- text $ "span" @: [hasClass "rank"]
                sTitle <- text $ "a"  @: [hasClass "storylink"]
                return (read idval ::Int,
                        EntryYC (nRank sRank) sTitle 0 0)

subtextsYC :: Scraper String [(Int, EntryYC)]
subtextsYC = chroots ("td" @: [hasClass "subtext"]) $ do
               href <- attr "href" $ "span" @: [hasClass "age"] // "a"
               sPoints <- text $ "span" @: [hasClass "score"]
               links <- texts $ "a" -- comments are the fourth link
               return (hrefToId href,
                       EntryYC 0 "" (nPoints sPoints) (nComments $ last links))

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

hrefToId :: String     -- ^ href has the form "item?id=NN"
         -> Int        -- ^ NN parsed as int
hrefToId h = read . drop 1 .snd $ break (=='=') h :: Int
