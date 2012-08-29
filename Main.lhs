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
> import System.Locale(defaultTimeLocale)
> import Network.HTTP(simpleHTTP,getResponseBody,getRequest)
> import Text.HTML.TagSoup
> import Data.Time.Format(readTime)
> import qualified Data.Time.Calendar as Cal
> import qualified Data.Map as M
> import Control.Monad.IO.Class

> main :: IO()
> main = do

Get arguments, we expect a url only, that is, program is to be called `trac-timeline-stats $url`.

>  args <- getArgs
>  case args of
>    [url] -> do
>      items <- getItems url
>      let 
>        tickets = frequenciesByDate $ filter isTicket items
>        wiki = frequenciesByDate $ filter isWikiPage items
>        chsets = frequenciesByDate $ filter isChangesetItem items
>        unks = frequenciesByDate $ filter isUnknownItem items
>      putStrLn $ "Tickets: "++ (show $ tickets)
>      putStrLn $ "Wiki: "++ (show $ wiki)
>      putStrLn $ "Changesets: "++ (show $ chsets)
>      putStrLn $ "Unknown: "++ (show $ unks)
>    _ -> putStrLn "?"

> data TicketStatus = ClosedTicket | CreatedTicket  deriving Show
> data WikiPageStatus = EditedWikiPage | CreatedWikiPage deriving Show

> data Item = Ticket {
>  tUser :: String
>  , tDate :: Cal.Day
>  , tTitle :: String
>  , tStatus :: TicketStatus
>  } | WikiPage {
>  wpUser :: String
>  , wpDate :: Cal.Day
>  , wpTitle :: String
>  , wpStatus :: WikiPageStatus
>  } | Changeset {
>    chsetUser :: String
>  , chsetDate :: Cal.Day
>  , chsetTitle :: String
> } | UnknownItem String deriving Show

> toDO :: a -> a
> toDO = id

> type URL = String

Fetch a list of items (both wiki articles and tickets) from a URL

> getItems :: URL -> IO [Item]
> getItems url = do
>   putStrLn "Fetching feed"
>   rawResp <- simpleHTTP $ getRequest url
>   putStrLn "Fetched feed"
>   rss <- getResponseBody rawResp
>   putStrLn "Parsing"
>--   putStrLn $ (show $ length $ partitions (~== "<item>") $ parseTags rss)
>   putStrLn "Calculating"
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
>   return $ mkItem title creator (readTime defaultTimeLocale "%a, %e %b %Y" (take 16 date) :: Cal.Day) category

Create an item from attributes

> type Category = String
> type Title = String
> type Creator = String

> mkItem :: Title -> Creator -> Cal.Day -> Category -> Item
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

We have a list of items which contain a date, let's count how many items happened by date.

> frequenciesByDate :: [Item] -> DateFrequencies
> frequenciesByDate = flip frequenciesByDate' M.empty

> type DateFrequencies = M.Map Cal.Day Int

> frequenciesByDate' :: [Item] -> DateFrequencies -> DateFrequencies
> frequenciesByDate' [] _ = M.empty
> frequenciesByDate' (x:xs) m = let
>   day = itemDate x
>   incFreq = (+) 1
>   updatedDateFreqs = case M.lookup day m of
>     Just _ ->  M.update (Just . incFreq) day m
>     _ -> M.insert day 1 m
>   nextDateFreqs = (frequenciesByDate xs)
>   in M.unionWith (+) updatedDateFreqs nextDateFreqs

> itemDate :: Item -> Cal.Day
> itemDate i | (isChangesetItem i) = chsetDate i
>            | (isWikiPage i)      = wpDate i
>            | (isTicket i)        = tDate i
>            | (isUnknownItem i)   = error "TODO: add date to unknown items"