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

> import Data.List(group,sortBy,intercalate,nub)
> import Data.Map(toList,keys)
> import Data.Function(on)
> import Data.Maybe(fromMaybe)

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
>        unks = frequenciesByDate $ filter isAnotherItem items
>      putStrLn $ "#########################"
>      putStrLn $ "#        REPORT "
>      putStrLn $ "#  (to plot with gnuplot)"
>      putStrLn $ "# Number of Tickets fetched: "++ (show $ sum $ M.elems tickets)
>      putStrLn $ "# Number of Wikipages edits fetched: "++ (show $ sum $ M.elems wiki)
>      putStrLn $ "# Number of Changesets (commits) fetched: "++ (show $ sum $ M.elems chsets)
>      putStrLn $ "# "
>      putStrLn $ "# Unknown: "++ (show $ unks)
>      --putStrLn $ "# freq tickets: "++ (show $ tickets)
>      --putStrLn $ "# freq wiki: "++ (show $ wiki)
>      --putStrLn $ "# freq comits: "++ (show $ chsets)
>      --putStrLn $ "# freq unk: "++ (show $ unks)
>      putStrLn $ "# date     ntickets      nwikis     ncommits     nothers"
>      mapM_ gnuplotPrintLn $ groupFreqsByDay [tickets,wiki,chsets,unks]
>    _ -> putStrLn "?"

Let's pretty print date frequencies for gnuplot. Prints how many times an item occured in a day

> gnuplotPrintLn :: (Cal.Day,[Int]) -> IO ()
> gnuplotPrintLn (d,itemTypesFreqs) = putStrLn $ (show d) ++ "    " ++ intercalate "            " (map show itemTypesFreqs)

> gnuplotPrint :: (Cal.Day,Int) -> IO ()
> gnuplotPrint (d,i) = putStr $ (show d) ++ " " ++ (show i)

Returns structured data to print the gnuplot data file. Elements given by a day represent the frequencies of each item type (e.g. tickets, wiki pages, etc.) in the given order of the list of date frequencies (first argument).

> groupFreqsByDay :: [DateFrequencies] -> [(Cal.Day,[Int])]
> groupFreqsByDay fs =
>   let 
>     days = nub $ concatMap keys fs
>     itemTypeFrequency :: Maybe Int -> Int
>     itemTypeFrequency = fromMaybe 0
>     freqsPerDay :: Cal.Day -> [DateFrequencies] -> [Int]
>     freqsPerDay d fs' = map (itemTypeFrequency . M.lookup d) fs'
>   in map (\d -> (d,freqsPerDay d fs)) days

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
> } | OtherItem {
>    itemType :: String
>  , otherItemTypeDate :: Cal.Day
>  } deriving Show

> toDO :: a -> a
> toDO = id

> type URL = String

Fetch a list of items (both wiki articles and tickets) from a URL

> getItems :: URL -> IO [Item]
> getItems url = do
>   putStrLn "## Debug output ## "
>   putStrLn "# Fetching feed.."
>   rawResp <- simpleHTTP $ getRequest url
>   putStrLn "# Fetched feed.."
>   rss <- getResponseBody rawResp
>   putStrLn "# Parsing.. "
>--   putStrLn $ (show $ length $ partitions (~== "<item>") $ parseTags rss)
>   putStrLn "# Calculating.."
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
>   a -> OtherItem a d
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

> isAnotherItem :: Item -> Bool
> isAnotherItem (OtherItem _ _) = True
> isAnotherItem _ = False

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
>            | (isAnotherItem i)   = otherItemTypeDate i

