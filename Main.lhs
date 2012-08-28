:{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse    #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Carlos López-Camey
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
-- Portability :  portable
--
--
-- A command line tool to generate statistics about a given trac timeline url.
-----------------------------------------------------------------------------

The following program generates statistics from a trac timeline.

> module Main where

> import           System.Environment
> import           System.Time

> import Network.HTTP(simpleHTTP,getResponseBody,getRequest)

> import Text.HTML.TagSoup

> import Control.Monad.Trans.Maybe
> import Control.Monad.IO.Class

> main :: IO()
> main = do

Get arguments, we expect a url only, that is, program is to be called `trac-timeline-stats $url`.

>  args <- getArgs
>  case args of
>    [url] -> do
>      items <- getItems url
>      let 
>        tickets = filter isTicket items
>        wiki = filter isWikiPage items
>        chsets = filter isChangesetItem items
>        unks = filter isUnknownItem items
>      putStrLn $ "Tickets: "++ (show $ length tickets)
>      putStrLn $ "Wiki: "++ (show $ length wiki)
>      putStrLn $ "Changesets: "++ (show $ length chsets)
>      putStrLn $ "Unknown: "++ (show $ unks)
>    _ -> putStrLn "?"

> data TicketStatus = ClosedTicket | CreatedTicket  deriving Show
> data WikiPageStatus = EditedWikiPage | CreatedWikiPage deriving Show

> data Item = Ticket {
>  tUser :: String
>  , tDdate :: Date
>  , tTitle :: String
>  , tStatus :: TicketStatus
>  } | WikiPage {
>  wpUser :: String
>  , wpDate :: Date
>  , wpTitle :: String
>  , wpStatus :: WikiPageStatus
>  } | Changeset {
>    chsetUser :: String
>  , chsetDate :: Date
>  , chsetTitle :: String
> } | UnknownItem String deriving Show

> toDO :: a -> a
> toDO = id

> type URL = String

Fetch a list of items (both wiki articles and tickets) from a URL

> getItems :: URL -> IO [Item]
> getItems url = do
>   rawResp <- simpleHTTP $ getRequest url
>--   putStrLn "getItems"
>   rss <- getResponseBody rawResp
>--   putStrLn $ (show $ length $ partitions (~== "<item>") $ parseTags rss)
>   return $ parseItems rss

Parse RSS (XML) to a list of items with the library tagsoup.

> parseItems :: String -> [Item]
> parseItems rss = do
>   items <- partitions (~== "<item>") $ parseTags rss   
>   (title,rss2) <- items `extractText` "<title>"
>   (creator,rss3) <- items `extractText` "<dc:creator>"
>   --(author,rss3) <- items `extractText` "<author>"
>   (date,rss3) <- items `extractText` "<pubDate>"
>   (category,rs) <- items `extractText` "<category>"
>   return $ mkItem title creator date category

Create an item from attributes

> type Category = String
> type Title = String
> type Date = String
> type Creator = String

> mkItem :: Title -> Creator -> Date -> Category -> Item
> mkItem t c d cat = case cat of
>   "newticket" -> Ticket c d t CreatedTicket
>   "closedticket" -> Ticket c d t ClosedTicket
>   "wiki" -> WikiPage c d t EditedWikiPage
>   "changeset" -> Changeset c d t
>   a -> UnknownItem a
> 

Given a parsed structure, we would like to substract information from the tags. This function looks for the first ocurrence of a tag and returns the text inside it together with the remaining structure.

> extractText :: [Tag String] -> String -> [(String,[Tag String])]
> extractText s t = 
>   case dropWhile (~/= t) s of
>     ((TagOpen _ _):(TagText t ):(TagClose _):s3) -> [(t,s3)]
>     _ -> [("",s)]

We need to separate tickets from wiki articles. The following two functions help filtering them from a list of items

Question: Is it possible to generalize on this?

> isTicket :: Item -> Bool
> isTicket (Ticket _ _ _ _) = True
> isTicket _ = False

> isWikiPage :: Item -> Bool
> isWikiPage (WikiPage _ _ _ _) = True
> isWikiPage _ = False

> isUnknownItem :: Item -> Bool
> isUnknownItem (UnknownItem s) = True
> isUnknownItem _ = False

> isChangesetItem :: Item -> Bool
> isChangesetItem (Changeset _ _ _) = True
> isChangesetItem _ = False