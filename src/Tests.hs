{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import ScrapeNewsYC
--import Main (requestedFilters)

main = htfMain htf_thisModulesTests

