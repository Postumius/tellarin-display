module Utilities exposing 
  ( ..
  )

import List as L
import Html as H exposing (Html, Attribute)
import Html.Attributes as A
import Dict as D exposing (Dict)
import Array as Arr exposing (Array)

nTimes : Int -> (a -> a) -> a -> a
nTimes n f acc =
  if n <= 0
  then acc
  else nTimes (n-1) f (f acc)

foldrP : (a -> (() -> b) -> b) -> b -> List a -> b
foldrP f bottom xxs =
  case xxs of
    [] -> bottom
    x::xs -> f x (\() -> foldrP f bottom xs)

searchWithIndex : (Int -> a -> Bool) -> List a -> Maybe a
searchWithIndex pred = 
  List.indexedMap Tuple.pair >>
  foldrP (\(i, x) searchRest -> 
    if pred i x then Just x else searchRest ()
  ) Nothing

get j = searchWithIndex (\i _ -> i == j)

search pred = searchWithIndex (\_ x -> pred x)

-- getWithDefault : Int -> a -> List a -> a
-- getWithDefault i dflt = get i >> Maybe.withDefault dflt

flip f a b = f b a

uncurry f (a, b) = f a b

getSet : (obj -> a) -> (a -> obj -> obj) -> (a -> a) -> obj -> obj
getSet getter setter f obj = 
  obj |> 
  getter |> 
  f |>
  flip setter obj
  
tryGetSet getter setter f obj =
  obj
  |> getter
  |> Maybe.map (f >> setter)
  |> Maybe.withDefault identity
  |> (|>) obj

type alias Promise a = () -> a

type Stream a
  = Empty
  | Cons a (Promise (Stream a))

repeat x = Cons x <| \() -> repeat x

mapWithList : (a -> b -> c) -> List a -> Stream b -> Stream c
mapWithList f list stream = 
  case (list, stream) of
    (x::xs, Cons y ysP) -> Cons (f x y) <| \() -> mapWithList f xs (ysP())
    _ -> Empty

toList stream =
  case stream of
    Empty -> []
    Cons x xsP -> x :: toList (xsP())

transpose : List (List a) -> List (List a)
transpose matrix = 
  L.foldr
    (L.map2 (::))
    ( matrix
      |> L.map L.length
      |> L.minimum
      |> Maybe.withDefault 0
      |> flip L.repeat []
    )
    matrix

--   if List.isEmpty ls
--   then []
--   else
--     ls
--     |> List.foldr (mapWithList (::)) (repeat [])
--     |> toList
-- 
-- rectangularise filler lists =
--   let 
--     lengths = List.map List.length lists
--     maxLength = lengths |> L.maximum |> Maybe.withDefault 0
--   in
--     List.map2 (\row length ->
--       row ++ 
--       L.repeat (maxLength - length) filler
--     ) lists lengths
-- 

maybe : b -> (a->b) -> Maybe a -> b
maybe default f = 
  Maybe.map f
  >> Maybe.withDefault default

filter : (a -> Bool) -> Maybe a -> Maybe a
filter pred mb =
  mb
  |> Maybe.andThen (\x ->
     if pred x then Just x else Nothing
  )

pop : Array a -> Maybe (Array a, a)
pop arr = 
  arr
  |> Arr.get (Arr.length arr - 1)
  |> Maybe.map (\last -> (Arr.slice 0 -1 arr, last))

dropBackWhile : (a -> Bool) -> Array a -> Array a
dropBackWhile pred arr = 
  arr
  |> pop
  |> filter (Tuple.second >> pred)
  |> maybe arr (Tuple.first >> dropBackWhile pred)

delete : Int -> Array a -> Array a
delete i arr =
  let 
    left =
      Arr.slice 0 i arr
    right =
      Arr.slice (i+1) (Arr.length arr) arr
  in Arr.append left right


type Lens s t a b =
  Lens
    { getter: s -> a
    , setter: b -> s -> t
    }

aL = 
  Lens
    { getter = .a
    , setter = \x obj -> { obj | a = x }
    }

bL = 
  Lens
    { getter = .b
    , setter = \x obj -> { obj | b = x }
    }

view (Lens {getter}) obj = getter obj

set (Lens {setter}) x obj = setter x obj

firstL =
  Lens
   { getter = Tuple.first
   , setter = \newA (a, b) -> (newA, b)
   }

inc n = n + 1

dec n = n - 1

minus n m = m - n

-- insertWith: (a->a->a) -> k -> a -> Dict k a -> Dict k a
-- insertWith f k newVal dict =
--   D.update (
--     Maybe.map (f newVal) 
--     >> Maybe.withDefault newVal
--     >> Just
--   ) dict
-- 
aloneInside container item = 
  item 
  |> L.singleton 
  |> container

wrapWith containers item =
  List.foldr aloneInside item containers

type Orientation = RowFirst | ColFirst

type alias TableParams msg =
    { tableAttrs: List (Attribute msg) 
    , headerAttrs: List (Attribute msg) 
    , bodyAttrs: List (Attribute msg) 
    , orientation: Orientation
    , matrix: List (List (Html msg))
    }

table : (TableParams msg -> TableParams msg) -> Html msg
table modify =
  let 
    defaultParams =
      TableParams
        []
        []
        []
        RowFirst
        []
    args = modify defaultParams
  in
    H.table args.tableAttrs (
      args.matrix
      |> L.indexedMap (\i ls -> 
         ls
         |> L.map (
            wrapWith 
              [ H.td (args |> if i == 0 then .headerAttrs else .bodyAttrs)
              ]
         )
      )
      |> (case args.orientation of 
            RowFirst -> identity
            ColFirst -> transpose
         )
      |> L.map (H.tr [])
    )
    
colTable matrix = 
  table (\defaultParams ->
    { defaultParams
    | headerAttrs = [ A.class "header-cell" ]
    , bodyAttrs = [ A.class "body-cell" ]
    , orientation = ColFirst
    , matrix = matrix
    }
  )

