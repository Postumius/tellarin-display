module Trie exposing (..)

import Dict as D exposing (Dict)
import NonEmptyDict as Ned exposing (Ned)
import Maybe as M
import Tuple as Tup

import Utilities as U
import Utilities.Maybe as MU
import Utilities.List as LU
import Sequence as Seq exposing (Sequence)

-- seqSearch = Seq.search Tuple.first

type alias Branch comparable a = ( comparable, (FruitTree comparable a) )

type alias Branches comparable a =
  Sequence (Branch comparable a)

type FruitTree comparable a
  = Fruiting a
  | Branching (Branches comparable a)
  | Both a (Branches comparable a)

getBranches tree = 
  case tree of
    Fruiting _ -> Nothing
    Branching branches -> Just branches
    Both _ branches -> Just branches

setBranches seq tree = 
  case tree of
    Fruiting x -> Both x seq
    Branching _ -> Branching seq
    Both x _ -> Both x seq

getFruit tree =
  case tree of
    Fruiting x -> Just x
    Branching _ -> Nothing
    Both x _ -> Just x

makeFruitTree mFruit mBranches = 
  case (mFruit, mBranches) of
    (Nothing, Nothing) -> Nothing
    (Just fruit, Nothing) -> Just <| Fruiting fruit
    (Nothing, Just branches) -> Just <| Branching branches
    (Just fruit, Just branches) -> Just <| Both fruit branches

type alias Rung comparable a = (Maybe a, Seq.Ladder (Branch comparable a))

type Ladder comparable a = 
  Ladder (List (Rung comparable a))
    -- (List (Rung comparable a))
    -- (Rung comparable a)

setSecond x (a, b) = (a, x)

over move (Ladder trail (mFruit, row)) =
  row
  |> move
  |> M.map (Tup.pair mFruit >> Ladder trail)

goLeft = over Seq.goLeft

goRight = over Seq.goRight

goDownOver intoRow (Ladder trail (mFruit, row))
  let 
    subTree =
      row
      |> Seq.getFocus
      |> Tup.second
    trailNew = (mFruit, row) :: trail
    mFruitNew = getFruit subTree
  in    
    MU.bind (subTree |> getBranches |> M.andThen intoRow) <| \rowNew ->
      Ladder trailNew (mFruitNew, rowNew)

goDownAt : comparable -> Ladder comparable a -> Maybe (Ladder comparable a)
goDownAt key = 
  goDownOver (Seq.find Tuple.first key)

goDownFirst =
  goDownOver (Seq.first >> Just)

goUp (Ladder ttts) = 
  MU.bind (LU.pop ttts) <| \((mFruit, row), tts) ->
  MU.bind (Lu.pop tts) <| \((mFruitUp, rowUp), ts) ->


    

-- Ladder trail focus =
--   Ladder
--     { trail = trail
--     , focus = focus
--     }
-- 
-- find : (List comparable) -> Ladder comparable a -> Maybe (Ladder comparable a)
-- find keys (Ladder {trail, tree} as ladder) =
--   case keys of
--     [] ->
--       Just ladder
--     k::ks ->
--       tree
--       |> getBranches
--       |> M.andThen (Ned.get k)
--       |> M.andThen (
--          jacobLadder (tree::trail)
--          >> find ks
--       )
-- 
find : (List comparable) -> Ladder comparable a -> Ladder comparable a
find keys (Ladder {trail, tree} as ladder) =
  Maybe.withDefault ladder <|
  MU.bind (LU.pop keys) <| \(k, ks) ->
  MU.bind (getBranches tree) <| \branches ->
  MU.bind (Ned.get k branches) <| \subTree ->
    jacobLadder (tree::trail) subTree
    |> find ks
    |> Just
  
type Trie comparable a =
  Trie (Maybe (FruitTree comparable a))

toLadder (Trie maybeTree) = 
  maybeTree
  |> M.map (Seq.singleton >> Ladder [])
