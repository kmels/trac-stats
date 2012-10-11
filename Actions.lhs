:{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse    #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Actions
-- Copyright   :  (c) Carlos López-Camey
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
-- Portability :  portable
--
--
-- Contains actions triggered by the main program.
-----------------------------------------------------------------------------

This function is called when the program is called with generate. It returns a string that is plottable with gnuplot

> module Actions where

> import Control.Monad(when)
> import qualified Data.Time.Calendar as Cal
> import Data.List(group,sort,intercalate,nub)
> import Data.Maybe(fromMaybe)
> import Data.Map(toList,keys)

> import qualified Data.Map as M
> import Types
> import Fetcher(getItems)
> import Item


> generate :: URL -> IO String
> generate url = do
>   items <- getItems url
>   when (null items) $ 
>       putStrLn "\nWARNING: I found no items, maybe you are not giving the *RSS* feed url?\n"
>   let
>     created_tickets = frequenciesByDate $ filter (\i -> isTicket i && tStatus i == CreatedTicket) items
>     closed_tickets = frequenciesByDate $ filter (\i -> isTicket i && tStatus i == ClosedTicket) items
>     wiki = frequenciesByDate $ filter isWikiPage items
>     chsets = frequenciesByDate $ filter isChangesetItem items
>     unks = frequenciesByDate $ filter isAnotherItem items
>     pre_report = "#########################\n"
>                ++ "#        REPORT\n"
>                ++ "#  (to plot with gnuplot)\n"
>                ++ "# Number of created tickets: "++ (show $ sum $ M.elems created_tickets)++"\n"
>                ++ "# Number of closed tickets: "++ (show $ sum $ M.elems closed_tickets)++"\n"
>                ++ "# Number of Wikipages edits fetched: "++ (show $ sum $ M.elems wiki)++"\n"
>                ++ "# Number of Changesets (commits) fetched: "++ (show $ sum $ M.elems chsets)++"\n"
>                ++ "# "++"\n"
>                ++ "# Unknown: "++ (show $ unks)++"\n"
>                ++ "# date     ntickets_created     ntickets_closed          nwikis             ncommits             nothers"++"\n"
>     report = concatMap gnuplotShow $ groupFreqsByDay [created_tickets,closed_tickets,wiki,chsets,unks]
>   return (pre_report ++ report)

> gnuplotShow :: (Cal.Day,[Int]) -> String
> gnuplotShow (d,itemTypesFreqs) = (show d) ++ "       " ++ intercalate "                    " (map show itemTypesFreqs) ++ "\n"

Returns structured data to print the gnuplot data file. Elements given by a day represent the frequencies of each item type (e.g. tickets, wiki pages, etc.) in the given order of the list of date frequencies (first argument).

> groupFreqsByDay :: [DateFrequencies] -> [(Cal.Day,[Int])]
> groupFreqsByDay fs =
>   let 
>     days = sort . nub $ concatMap keys fs
>     itemTypeFrequency :: Maybe Int -> Int
>     itemTypeFrequency = fromMaybe 0
>     freqsPerDay :: Cal.Day -> [DateFrequencies] -> [Int]
>     freqsPerDay d fs' = map (itemTypeFrequency . M.lookup d) fs'
>   in map (\d -> (d,freqsPerDay d fs)) days

We have a list of items which contain a date, let's count how many items happened by date.

> type DateFrequencies = M.Map Cal.Day Int

> frequenciesByDate :: [Item] -> DateFrequencies
> frequenciesByDate = flip frequenciesByDate' M.empty

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


