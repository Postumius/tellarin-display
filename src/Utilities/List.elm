module Utilities.List exposing (..)

import Maybe as M
import List exposing (..)
import Utilities.Maybe as MU

pop : List a -> Maybe (a, List a)
pop xxs =
  MU.bind (head xxs) <| \x ->
  MU.bind (tail xxs) <| \xs ->
    Just (x, xs)
