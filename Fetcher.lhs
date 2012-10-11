:{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse    #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Fetcher
-- Copyright   :  (c) Carlos López-Camey
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
-- Portability :  portable
--
--
-- Fetches data and structures it
-----------------------------------------------------------------------------

> module Fetcher where

> import Network.HTTP(simpleHTTP,getResponseBody,getRequest)
> import Text.HTML.TagSoup
> import Data.Time.Format(readTime)
> import System.Locale(defaultTimeLocale)
> import qualified Data.Time.Calendar as Cal

> import Types
> import Item

Fetch a list of items (both wiki articles and tickets) from a URL

> getItems :: URL -> IO [Item]
> getItems url = do
>   putStrLn "## Debug output ## "
>   putStrLn "# Fetching feed.."
>   rawResp <- simpleHTTP $ getRequest url
>   putStrLn "# Fetched feed.."
>   rss <- getResponseBody rawResp
>   putStrLn "# Parsing.. "
>   return $ parseItems rss

Parse RSS (XML) to a list of items with the library tagsoup.

> parseItems :: String -> [Item]
> parseItems rss = do
>   (trac_version,rss') <- parseTags rss `extractText` "<generator>"
>   items <- partitions (~== "<item>") $ rss'
>   (title,_) <- items `extractText` "<title>"
>   (date,_) <- items `extractText` "<pubDate>"
>   (category,_) <- items `extractText` "<category>"
>   (creator,_) <- items `extractText` case category of
>     "changeset" -> "<author>"
>     _ -> "<dc:creator>"
>   return $ mkItem title creator (readTime defaultTimeLocale "%a, %e %b %Y" (take 16 date) :: Cal.Day) category

Given a parsed structure, we would like to substract information from the tags. This function looks for the first ocurrence of a tag and returns the text inside it together with the remaining structure.

> extractText :: [Tag String] -> String -> [(String,[Tag String])]
> extractText s t = 
>   case dropWhile (~/= t) s of
>     ((TagOpen _ _):(TagText t ):(TagClose _):s3) -> [(t,s3)]
>     _ -> [("",s)]

