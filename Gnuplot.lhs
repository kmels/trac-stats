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
-- Generates output to be plotted with gnuplot
-----------------------------------------------------------------------------

This function is called when the program is called with generate. It returns a string that is plottable with gnuplot

> module Gnuplot where

> import Control.Monad(when)
> import qualified Data.Time.Calendar as Cal
> import Data.List(group,sort,intercalate,nub)
> import Data.Maybe(fromMaybe)
> import Data.Map(toList,keys)

> import qualified Data.Map as M
> import Types
> import Fetcher(getItems)
> import Item
> import Manipulate

> gnuplot :: URL -> IO String
> gnuplot url = do
>   let commentF cmmt = "# " ++ cmmt ++ " #"
>   items <- getItems commentF url
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

