module NonEmptyDict exposing (..)

import Dict as D exposing (Dict)
import Utilities.Dict as DU 

type Ned comparable a = 
  Ned comparable a (Dict comparable a)

get key (Ned k v d) =
  if key == k
  then Just v
  else D.get key d

fromDict =
  DU.pop
  >> Maybe.map (\(k, v, d) -> Ned k v d)

update key f (Ned k v d) = 
  d
  |> D.insert k v
  |> D.update key f
  |> fromDict
