module CardNameCompressor where

import Text.Regex
import Control.Arrow (first)

-- Card Name to Abbr. Map

cnMap :: [(String,String)]
cnMap = [ ("Chrome Mox", "Mox")
        , ("Empty the Warrens", "EtW")
        , ("Rending Volley", "RV")
        , ("Tendrils of Agony", "ToA")
        , ("Infernal Tutor", "IT")
        , ("Cabal Therapy", "Therapy")
        , ("Burning Wish", "Wish")
        , ("Ad Nauseam", "AdNaus")
        , ("Perilous Voyage", "PV")
        , ("Gitaxian Probe", "Probe")
        , ("Echoing Truth", "Truth")
        , ("Past in Flames", "PiF")
        , ("Grapeshot", "Grape")
        ]

cnRegex :: [(Regex, String)]
cnRegex = map (first mkRegex) cnMap

cnFindAndReplace :: String -> [(Regex, String)] -> String
cnFindAndReplace s [] = s
cnFindAndReplace s (x:xs) = cnFindAndReplace r xs
    where r = subRegex (fst x) s (snd x)

cnReplaceNames :: String -> String
cnReplaceNames s = cnFindAndReplace s cnRegex