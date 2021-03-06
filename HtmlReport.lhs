> {-# LANGUAGE TemplateHaskell #-}

----------------------------------------------------------------------------
-- |
-- Module      :  HtmlReport
-- Copyright   :  (c) Carlos López-Camey
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
-- Portability :  portable
--
--
-- Renders a html file with in form of graphics.
-----------------------------------------------------------------------------

> module HtmlReport where

> import qualified Data.Map as M
> import qualified Data.Text as T
> import Data.Map(toList,keys)
> import Data.List(group,sort,intercalate,nub)
> import Data.Maybe(fromMaybe)
> import Data.Map(toList,keys)
> import System.Directory(getCurrentDirectory)
> import System.IO.Unsafe(unsafePerformIO)
> import Data.Aeson

> import Text.Blaze.Html.Renderer.String (renderHtml)
> import Text.Hamlet
> import Text.Julius
> import Types
> import Item
> import Fetcher
> import Manipulate

Let's render html from a template

> htmlReport :: URL -> IO String
> htmlReport url = do
>   let commentF cmmt = "<!-- " ++ cmmt ++ "-->"
>   items <- getItems commentF url
>   let messages = if (null items) 
>                  then ""
>                  else "\nWARNING: I found no items, maybe you are not giving the *RSS* feed url?\n"
>   let
>     (created_tickets_days,created_tickets_freqs) = unzip . M.toList . accummulate . frequenciesByDate $ filter (\i -> isTicket i && tStatus i == CreatedTicket) items
>     (closed_tickets_days,closed_tickets_freqs) = unzip . M.toList . accummulate . frequenciesByDate $ filter (\i -> isTicket i && tStatus i == ClosedTicket) items
>     wiki = frequenciesByDate $ filter isWikiPage items
>     chsets = frequenciesByDate $ filter isChangesetItem items
>     unks = frequenciesByDate $ filter isAnotherItem items 
>     pre_report = "   REPORT "
>                ++ "- Number of created tickets: "++ (show $ sum $ created_tickets_freqs)++"\n"
>                ++ "- Number of closed tickets: "++ (show $ sum $ closed_tickets_freqs)++"\n"
>                ++ "- Number of Wikipages edits fetched: "++ (show $ sum $ M.elems wiki)++"\n"
>                ++ "- Number of Changesets (commits) fetched: "++ (show $ sum $ M.elems chsets)++"\n"
>                ++ "- Unknown: "++ (show $ unks)++"\n"
>     template = $(shamletFile $ (unsafePerformIO getCurrentDirectory) ++ "/templates/html-report.hamlet")
>     
>   putStrLn $ commentF pre_report  
>   return $ renderHtml template
