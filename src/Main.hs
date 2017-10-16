module Main where

import Text.XML.HXT.Core
import Text.HandsomeSoup
import Data.Semigroup ((<>))
import Data.List

tesSideboardGuide :: String
tesSideboardGuide = "http://theepicstorm.com/sideboarding-guide/"

data WordType s = Count s | Cardname s | None s deriving Show

toWordType :: String -> WordType String
toWordType s
  | isPrefixOf "\n" s || isPrefixOf "-" s || isPrefixOf "+" s = Count s
  | s == "None." = None s
  | otherwise = Cardname s

toWordTypes :: [String] -> [WordType String]
toWordTypes = map toWordType

wtToString :: WordType String -> String
wtToString (Count s) = s
wtToString (Cardname s) = s
wtToString (None s) = s

combineCardNames :: [WordType String] -> [WordType String]
combineCardNames [] = []
combineCardNames [a] = [a]
combineCardNames (x@(None _):xs) = x : combineCardNames xs
combineCardNames (x@(Count s):y:xs) = case y of
  Cardname n -> combineCardNames (Count (s ++ n) : xs)
  _ -> x : combineCardNames (y:xs)

-- card header pattern is \n[+-]\d+\s
-- Special case is "None."
-- if we start with a card header, the next items are card names until we hit either "None." or a new header
buildBoardInfo :: [String] -> [String]
buildBoardInfo = map wtToString . combineCardNames . toWordTypes

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
  decks <- runX $ doc >>> css ".clearfix .col-md-4 h5, .clearfix .col-md-3 h5" //> getText
  cards <- runX $ doc >>> css ".clearfix .col-md-4 p, .clearfix .col-md-3 p" //> getText
  let cards' = groupCardInfo . buildBoardInfo $ cards
  let entries = zip decks cards'
  let entries' = map formatEntry entries

  writeFile "sideboard.html" $ intercalate "\n" entries'