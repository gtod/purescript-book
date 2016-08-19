module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

-- Ex 3.2
findByAddress :: Address -> AddressBook -> Maybe Entry
findByAddress address = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.address.street == address.street &&
                      entry.address.city == address.city &&
                      entry.address.state == address.state

-- Ex 3.3
hasName :: String -> String -> AddressBook -> Boolean
hasName firstName lastName book = not null $ filter filterEntry book
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

-- Ex 3.4
removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy matching
  where
    matching :: Entry -> Entry -> Boolean
    matching a b = a.firstName == b.firstName && a.lastName == b.lastName
