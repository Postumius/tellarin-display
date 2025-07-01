module Utilities.Maybe exposing (..)

import Maybe as M

import Utilities.Function as FU

maybe : b -> (a->b) -> Maybe a -> b
maybe default f =
  Maybe.map f
  >> Maybe.withDefault default

bind = FU.flip M.andThen

lift = FU.flip M.map
