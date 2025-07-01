module TellarinDate exposing 
  ( seasonNames
  , dayNames
  , Date
  , Base (..)
  , baseNumber
  , DenomModifier
  , TimeDenomination
  , get
  , startGetter
  , startSetter
  , toString
  , baseDenominations
  , denoms
  , epoch
  , plus
  , makeDate
  , encode
  , decoder
  )

import Json.Encode as E
import Json.Decode as D exposing (Decoder)
import Tuple as Tup
import Dict as Dict exposing (Dict)

import Utilities as U
import Utilities.Function as FU
import NonEmpty as NE exposing (NonEmpty(..))

seasonNames = ["Seed", "Harvest", "Scorch", "Frost"]

cyclesPerSeason = 9

dayNames = 
  [ "Silth", "Est", "Kross", "Oust", "Rest", "Streth"
  , "Eith", "Kesh", "Orseh", "Rath"
  ]

hoursPerDay = 24

minutesPerHour = 60

type Date = Date (NonEmpty Int)

type Base = Names (List String) | Number Int

baseNumber base =
  case base of
    Names ls -> List.length ls
    Number n -> n

type alias TimeDenomination =
  { name: String
  , startAt: Int
  , base: Maybe Base
  , getter: Date -> Int
  , setter: Int -> Date -> Date
  , modifier: DenomModifier
  }

overflowDenomination = 
     ("year",   1)
baseDenominations =
  [ (("season", 1), Names seasonNames)
  , (("cycle" , 1), Number cyclesPerSeason) 
  , (("day"   , 1), Names dayNames) 
  , (("hour"  , 0), Number hoursPerDay) 
  , (("minute", 0), Number minutesPerHour) 
  ]

denoms : NonEmpty (TimeDenomination)
denoms = 
  NE
    (overflowDenomination, Nothing)
    (List.map (Tuple.mapSecond Just) baseDenominations) |>
  NE.indexedMap (\i ((name, startAt), base) ->
    let
      getter (Date ne) = 
        NE.get i ne 
        -- |> Maybe.map ((+) startAt)
        |> Maybe.withDefault -1
      setter n (Date ne) = 
        NE.set i n ne -- (n - startAt) ne 
        |> makeDate
    in 
      TimeDenomination name startAt base getter setter (U.getSet getter setter)
  )

denomsIndex = 
  denoms
  |> NE.toList
  |> List.indexedMap (\i denom -> (denom.name, i))
  |> Dict.fromList

get name date = 
  denomsIndex 
  |> Dict.get name
  |> Maybe.andThen (FU.flip NE.get denoms)
  |> Maybe.map (.getter >> (|>) date)

startGetter denom = denom.getter >> (+) denom.startAt

startSetter denom n = denom.setter (n - denom.startAt)

toString (Date ne) =
  let
    stringDict =
      NE.map2 (\denom n ->
        ( denom.name
        , case denom.base of
            Just (Names names) -> 
              names 
              |> U.get n
              |> Maybe.withDefault "(number out of range)"
            _ -> n + denom.startAt |> String.fromInt
        )
      ) denoms ne
      |> NE.toList
      |> Dict.fromList
    d name = 
      Dict.get name stringDict
      |> Maybe.withDefault "(denom not found!)"
    suffix str = 
      str ++
      if String.slice -2 -1 str == "1" then
        "th"
      else if String.endsWith "1" str then
        "st"
      else if String.endsWith "2" str then
        "nd"
      else if String.endsWith "3" str then
        "rd"
      else
        "th"
    padWithZeros str = 
      String.repeat (2 - String.length str) "0" ++ str
  in
    suffix (d "cycle")
    ++ " "
    ++ d "day"
    ++ " of "
    ++ d "season"
    ++ ", "
    ++ d "year"
    ++ " yor, "
    ++ padWithZeros (d "hour")
    ++ ":"
    ++ padWithZeros (d "minute")

baseNumbers = 
  baseDenominations |>
  List.map (\(_, base) ->
    case base of 
      Names ls -> List.length ls
      Number n -> n
  ) 

type alias DenomModifier = (Int -> Int) -> Date -> Date

epoch : Date
epoch = 
  denoms |>
  NE.map (always 0) |>
  Date
  
multiBaseAddition bases nDigits mDigits =
  List.map3 (\a b c -> (a, b, c)) bases nDigits mDigits |>
  List.foldr (\(base, nDigit, mDigit) {carry, sumDigits} ->
    let 
      addition = nDigit + mDigit + carry
      modulo = addition |> modBy base
    in 
      { carry = (addition-modulo) // base
      , sumDigits = modulo :: sumDigits
      }
  ) { carry = 0, sumDigits = [] }
 
plus : Date -> Date -> Date
plus (Date (NE year1 rest1)) (Date (NE year2 rest2)) =
  let 
    { carry, sumDigits } = multiBaseAddition baseNumbers rest1 rest2
  in 
    NE (year1 + year2 + carry) (sumDigits) |>
    Date

makeDate : NonEmpty Int -> Date 
makeDate = Date >> plus epoch

encode : Date -> E.Value
encode (Date ns) = ns |> NE.encode E.int

decoder : Decoder Date
decoder = NE.decoder D.int |> D.map makeDate
