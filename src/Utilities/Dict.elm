module Utilities.Dict exposing (..)

import Dict exposing (..)

import Utilities.Maybe as MU

head =
  foldr (\k v _ -> Just (k,v)) Nothing

pop : Dict comparable v -> Maybe (comparable, v, Dict comparable v)
pop d =
  MU.lift (head d) <| \(k, v) ->
  (k, v, remove k d)
