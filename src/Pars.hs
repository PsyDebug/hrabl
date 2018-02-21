module Pars
    ( repars
    ) where

import Text.XML.HXT.Core
import Text.HandsomeSoup
import Data.Char (isSpace, toLower)
import Data.List (map, dropWhile, reverse, filter, isInfixOf)
reGet :: IO [[String]]
reGet = do
  {-Change url-}
  let doc = fromUrl "http://www.bbc.com/sport/winter-olympics/medals/countries"
  trget <- runX $ doc >>> multi (hasName "tr")
  let listget x = x >>= runLA (multi (hasName "td") //> getText)
  let fg = [listget [fir] | fir <- trget]
  return $ fg

repars :: String -> IO ()
repars ar =  do
  rows <- reGet
  let getrow = map (unwords . tail'' . map (dropWhile isSpace . reverse . dropWhile isSpace . reverse)) rows
  mapM_ putStrLn $ filter (isInfixOf''  ar) getrow


tail'' :: [a] -> [a]
tail'' [] = []
tail'' (x:xs) = xs

isInfixOf'' x row = isInfixOf (map toLower x) (map toLower row)
