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
-- The following program generates statistics from a trac timeline.
-----------------------------------------------------------------------------

> module TracStats.Main where

> import           System.Environment
> import           System.Time
> import Control.Monad.IO.Class

> import Data.Function(on)

> import Actions

> main :: IO()
> main = do

Get arguments, we expect a url only, that is, program is to be called `trac-timeline-stats $url`.

>  args <- getArgs
>  case args of
>    ["gnuplot",url] -> do
>      gnuRawData <- generate url
>      putStrLn gnuRawData
>    ["html-report",filename url]
>    _ -> putStrLn "Wrong arguments. Usage: trac-stats $action $url, where $url is an trac timeline feed URL surrounded by quotes and $action is either `gnuplot` or `html-report $filename.html`"

Given a day and a list of frequencies corresponding to an item type, this function pretty prints it in a line.

