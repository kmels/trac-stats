:{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse    #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Item
-- Copyright   :  (c) Carlos López-Camey
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
-- Portability :  portable
--
--
-- The following data type represents an entity in Trac.
-----------------------------------------------------------------------------

> module Item where

> import qualified Data.Time.Calendar as Cal

> data TicketStatus = ClosedTicket | CreatedTicket  deriving (Show,Eq)
> data WikiPageStatus = EditedWikiPage | CreatedWikiPage deriving (Show,Eq)

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


The following program generates statistics from a trac timeline.


> type Category = String
> type Title = String
> type Creator = String

Create an item from attributes

> mkItem :: Title -> Creator -> Cal.Day -> Category -> Item
> mkItem t c d cat = case cat of
>   "newticket" -> Ticket c d t CreatedTicket
>   "closedticket" -> Ticket c d t ClosedTicket
>   "wiki" -> WikiPage c d t EditedWikiPage
>   "changeset" -> Changeset c d t
>   a -> OtherItem a d
> 

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

> itemDate :: Item -> Cal.Day
> itemDate i | (isChangesetItem i) = chsetDate i
>            | (isWikiPage i)      = wpDate i
>            | (isTicket i)        = tDate i
>            | (isAnotherItem i)   = otherItemTypeDate i

