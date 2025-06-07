module DateTrie exposing (..)

import Dict as D exposing (Dict)
import List as L

import Utilities as U
import TellarinDate as Date exposing (Date)
import NonEmpty as NE

trieLevels = 
  Date.baseDenominations
  |> L.map (Tuple.second >> Date.baseNumber)
  
type TimeUnit = TimeUnit (List Int)

toTimeUnit : Int -> Date -> TimeUnit
toTimeUnit depth date =
  Date.denoms
  |> NE.tail
  |> L.take depth
  |> L.map (.getter >> (|>) date)
  |> TimeUnit

type alias Event =
  { onYear: Int -> Bool
  }

type DateTrie = 
  DateTrie 
    { leaves: Dict String Event
    , branches: Dict Int DateTrie
    }

dateTrie leaves branches =
  DateTrie
    { leaves = leaves
    , branches = branches
    }

emptyTrie = 
  dateTrie D.empty D.empty

toPair (DateTrie { leaves, branches }) = (leaves, branches)

insert : TimeUnit -> String -> Event -> DateTrie -> DateTrie
insert (TimeUnit keys) name event trie =
  case (keys, trie |> toPair) of
    ( [], (leaves, branches) ) ->
      leaves
      |> D.insert name event
      |> U.flip dateTrie branches
    ( k::ks, (leaves, branches) ) ->
      branches
      |> D.update k (
         Maybe.withDefault emptyTrie
         >> insert (TimeUnit ks) name event
         >> Just
      ) 
      |> dateTrie leaves
