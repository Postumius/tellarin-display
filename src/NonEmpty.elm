module NonEmpty exposing (..)

import Json.Encode as E
import Json.Decode as D exposing (Decoder)

import Utilities.Function as FU
-- import NonZero as NZ exposing ( NonZero(..) )

type NonEmpty a = NE a (List a)

head (NE x _) = x

tail (NE x xs) = xs

toList (NE x xs) = x::xs

foldli : (a->b) -> (a->b->b) -> NonEmpty a -> b
foldli init recur (NE x xs) =
  List.foldl recur (init x) xs

foldl recur acc = foldli (FU.flip recur acc) recur

singleton x = NE x []

cons x0 (NE x1 xs) = NE x0 (x1::xs)

reverse = foldli singleton cons 

map : (a->b) -> NonEmpty a -> NonEmpty b
map f (NE x xs) = NE (f x) (List.map f xs)

map2 f (NE x xs) (NE y ys) = NE (f x y) (List.map2 f xs ys)

indexedMap f (NE x xs)= 
  NE (f 0 x) (List.indexedMap ((+)1 >> f) xs)

-- take : NonZero -> NE a -> NE a
-- take n xs =
--   case (n, xs) of
--     (One, NE x _) -> singleton x
--     (_, NE x [] -> singleton x
--     (Inc m, NE x0 (x1::xs)) -> cons x0 take m (NE x1 xs)
-- 
search pred = 
  foldl (\x acc -> 
    if pred x then Just x else acc
  ) Nothing

get: Int -> NonEmpty a -> Maybe a
get i = 
  indexedMap Tuple.pair >>
  search (Tuple.first >> (==) i) >>
  Maybe.map Tuple.second

set i y = 
  let replacer (j, x) = if i == j then y else x
  in
    indexedMap Tuple.pair >>
    reverse >>
    foldli 
      (replacer >> singleton)
      (replacer >> cons)

encode : (a -> E.Value) -> NonEmpty a -> E.Value
encode encodeA (NE first rest) =
  E.object
    [ ("first", encodeA first)
    , ("rest", E.list encodeA rest)
    ]

decoder : Decoder a -> Decoder (NonEmpty a)
decoder aDecoder = 
  D.map2 NE
    (D.field "first" aDecoder)
    (D.field "rest" <| D.list aDecoder)

