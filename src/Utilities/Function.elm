module Utilities.Function exposing (..)

nTimes : Int -> (a -> a) -> a -> a
nTimes n f acc =
  if n <= 0
  then acc
  else nTimes (n-1) f (f acc)

flip f a b = f b a

uncurry f (a, b) = f a b


