{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import Data.List (sort,sortBy)
import Data.Function (on)

import ScrapeNewsYC
import ProcessNewsYC (requestedFilters)

main = htfMain htf_thisModulesTests

test_empty = assertEqual ([] :: [EntryYC], [] :: [EntryYC]) (requestedFilters [])

prop_repeatable :: [(Int,String,Int,Int)] -> Bool
prop_repeatable abcds = and checks
    where es = map makeEntryYC abcds
          (a,b) = requestedFilters es
          (a',b') = requestedFilters a
          (a'',b'') = requestedFilters b
          checks = [a' == a
                   ,b''== b
                   ,null b'
                   ,null a'' ]

prop_moreThanFiveWords :: [(Int,String,Int,Int)] -> Bool
prop_moreThanFiveWords abcds =
    all ((>5).length.words.title) longs
        where longs = fst.requestedFilters.map makeEntryYC $ abcds

prop_fiveWordsOrFewer :: [(Int,String,Int,Int)] -> Bool
prop_fiveWordsOrFewer abcds =
    all ((<=5).length.words.title) shorts
        where shorts = snd.requestedFilters.map makeEntryYC $ abcds

prop_sortedByComments :: [(Int,String,Int,Int)] -> Bool
prop_sortedByComments abcds = longComs == sort longComs
        where longs = fst.requestedFilters.map makeEntryYC $ abcds
              longComs = map commented longs

prop_sortedByPoints :: [(Int,String,Int,Int)] -> Bool
prop_sortedByPoints abcds = shortPts == sort shortPts
        where shorts = snd.requestedFilters.map makeEntryYC $ abcds
              shortPts = map points shorts

prop_closure :: [(Int,String,Int,Int)] -> Bool
prop_closure abcds = srt es == srt es'
    where es = map makeEntryYC abcds
          es' = a ++ b
          (a,b) = requestedFilters es
          srt = sortBy (compare `on` hackhash)
          hackhash e = (rank e, title e, points e, commented e)
