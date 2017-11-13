{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import ScrapeNewsYC
import ProcessNewsYC (requestedFilters)

main = htfMain htf_thisModulesTests

prop_repeatable :: [(Int,String,Int,Int)] -> Bool
prop_repeatable abcds = all id checks
    where es = map makeEntryYC abcds
          (a,b) = requestedFilters es
          (a',b') = requestedFilters a
          (a'',b'') = requestedFilters b
          checks = [a' == a
                   ,b'== []
                   ,b''== b
                   ,a''== [] ]
