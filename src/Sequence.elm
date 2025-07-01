module Sequence exposing (..)

import Maybe as M

import Utilities as U
import Utilities.Maybe as MU
import NonEmpty as NE exposing (NonEmpty(..))

type Sequence a = 
  Sequence (NonEmpty a)

singleton = NE.singleton >> Sequence

type Ladder a =
  Ladder (List a) a (List a)

take (Ladder ls x rs) = 
  (x, \x1 -> Ladder ls xs rs)

getFocus (Ladder _ f _) = f

setFocus f (Ladder ls _ rs) = Ladder ls f rs

goLeft (Ladder lls f rs) = 
  case lls of
    [] -> Nothing
    l::ls -> Just <| Ladder ls l (f::rs)

goRight (Ladder ls f rrs) = 
  case rrs of
    [] -> Nothing
    r::rs -> Just <| Ladder (f::ls) r rs

find : (a -> comparable) -> a -> Sequence a -> Maybe Ladder a
find toComparable x (Sequence (NE r rs)) =
  let 
    recur ladder =
      if U.compare toComparable (==) (getFocus ladder) x
      then Just ladder
      else ladder |> goRight |> M.andThen recur
  in
    recur (Ladder [] r rs)

first : Sequence a -> Ladder a
first (Sequence (NE r rs)) =
  Ladder [] r rs

leave : (Ladder a) -> (Sequence a)
leave ladder = 
  ladder
  |> goLeft
  |> MU.maybe 
     (ladder |> \(Ladder _ f rs) -> NE f rs |> Sequence)
     leave
