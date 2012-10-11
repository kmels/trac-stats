:{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse    #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Manipulate
-- Copyright   :  (c) Carlos López-Camey
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
-- Portability :  portable
--
--
-- Manipulates items data, like grouping by date.
-----------------------------------------------------------------------------

> module Manipulate where

> import qualified Data.Time.Calendar as Cal
> import Data.List(group,sort,intercalate,nub)
> import Data.Maybe(fromMaybe)
> import Data.Map(toList,keys)
> import qualified Data.Map as M

> import Types
> import Fetcher(getItems)
> import Item

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


