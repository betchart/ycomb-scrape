module Main where

import ProcessNewsYC (processNewsYC)

main :: IO ()
main = processNewsYC "https://news.ycombinator.com"
