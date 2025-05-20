module TellarinDate exposing 
  ( seasonNames
  , dayNames
  , Date
  , Base (..)
  , DenomModifier
  , TimeDenomination
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

import Utilities as U
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

overflowDenomination = 
     "year"
baseDenominations =
  [ ("season", Names seasonNames)
  , ("cycle" , Number cyclesPerSeason) 
  , ("day"   , Names dayNames) 
  , ("hour"  , Number hoursPerDay) 
  , ("minute", Number minutesPerHour) 
  ]

baseNumbers = 
  baseDenominations |>
  List.map (\(_, base) ->
    case base of 
      Names ls -> List.length ls
      Number n -> n
  ) 

type alias DenomModifier = (Int -> Int) -> Date -> Date

type alias TimeDenomination =
  { name: String
  , base: Maybe Base
  , getter: Date -> Int
  , setter: Int -> Date -> Date
  , modifier: DenomModifier
  }

denoms = 
  NE
    (overflowDenomination, Nothing)
    (List.map (Tuple.mapSecond Just) baseDenominations) |>
  NE.indexedMap (\i (name, base) ->
    let
      getter = \(Date ne) -> NE.get i ne |> Maybe.withDefault -1
      setter = \x (Date ne) -> NE.set i x ne |> makeDate
    in 
      TimeDenomination name base getter setter (U.getSet getter setter)
  )

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
