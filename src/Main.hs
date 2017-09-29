module Main where

import Text.XML.HXT.Core
import Text.HandsomeSoup
import Data.Semigroup ((<>))
import Data.List

tesSideboardGuide :: String
tesSideboardGuide = "http://theepicstorm.com/sideboarding-guide/"

flattenCardInfo :: [String] -> [String]
flattenCardInfo [] = []
flattenCardInfo (x:y:xs) = (x ++ y) : flattenCardInfo xs

groupCardInfo :: [String] -> [[String]]
groupCardInfo [] = []
groupCardInfo (x:xs) = (x : ys) : groupCardInfo zs
  where (ys, zs) = span ((== '\n') . head) xs

formatCards :: [String] -> String
formatCards = filter ('\n' /=) . intercalate ", "

formatEntry :: (String, [String]) -> String
formatEntry (d,c) = "<p><strong>" <> d <> "</strong> " <> formatCards c <> "</p>"

main :: IO ()
main = do
  let doc = fromUrl tesSideboardGuide
  decks <- runX $ doc >>> css ".clearfix .col-md-4 h5" //> getText
  cards <- runX $ doc >>> css ".clearfix .col-md-4 p" //> getText
  let cards' = groupCardInfo . flattenCardInfo $ cards
  let entries = zip decks cards'
  let entries' = map formatEntry entries

  writeFile "sideboard.html" $ intercalate "\n" entries'