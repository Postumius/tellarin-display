port module Display exposing (..)

import Browser
import Html exposing (..)
import Json.Decode as D
import Maybe
import Result
import Dict exposing (Dict)
import Array as Arr exposing (Array)

import Controls exposing (..)
import Utilities exposing (..)
import TellarinDate as Date exposing (Date)
import NonEmpty as NE exposing (NonEmpty(..))

main =
  Browser.element 
    { init = init
    , update = update
    , view = displayView >> Html.map never
    , subscriptions = subscriptions
    }

type alias DisplayModel =
  { textFields : Dict Key String
  , activeTab : Int
  , date : Date
  , nCombatRows: Int
  }


type alias Model = Result D.Error DisplayModel

init : () -> ( Model, Cmd Msg )
init _ =
  ( Ok 
      { activeTab = 0
      , textFields = Dict.singleton "teext" ""
      , date = Date.epoch
      , nCombatRows = 0
      }
  , Cmd.none
  )

type Msg 
  = Receive D.Value 

-- decode : D.Value -> Model
-- decode =
--   let
--     dInfo =
--       D.map3 Info
--         (D.field "activeTab" D.int)
--         (D.field "textFields" <| D.dict D.string)
--         (D.field "date" Date.decoder)
--   in
--     D.decodeValue dInfo

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Receive modelVal ->
      (decode DisplayModel modelVal, Cmd.none)

port elmReceiver : (D.Value -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions _ =
  elmReceiver Receive

tabErrorView _ = text "active tab not found"

nowView: DisplayModel -> Html Never
nowView info =
  div []
    [ h2 [] [ text "Now" ]
    , text <| getTextField "teext" info
    , text <| Date.toString info.date
    ]

combatView: DisplayModel -> Html Never
combatView info =
  let
    suffix i str = str ++ (String.fromInt i)
    rows =
      List.range 1 info.nCombatRows
      |> List.map (\i ->
        div []
          [ text (getTextField (suffix i "name") info ++ "  ")
          , text (getTextField (suffix i "AC") info)
          ]
      )
  in
    [ h2 [] [ text "Combat" ] ]
    ++ rows
    |> div []

displayView : Result D.Error DisplayModel -> Html Never
displayView model =
  case model of
    Err e ->
      div [] [ e |> D.errorToString |> text ]
    Ok info -> 
      get info.activeTab 
        [ nowView
        , combatView
        ] |>
      Maybe.withDefault tabErrorView |>
      (|>) info
