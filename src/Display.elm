port module Display exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Json.Decode as D
import Maybe
import Dict exposing (Dict)
import Array as Arr exposing (Array)

import Controls exposing (..)
import Utilities as U
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
  , combatRows : Array (Array String)
  , nVisibleRows: Int
  }


type alias Model = Result D.Error DisplayModel

init : () -> ( Model, Cmd Msg )
init _ =
  ( Ok 
      { activeTab = 0
      , textFields = Dict.singleton "teext" ""
      , date = Date.epoch
      , combatRows = Arr.empty
      , nVisibleRows = 0
      }
  , Cmd.none
  )

type Msg 
  = Receive D.Value 

update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
  case msg of
    Receive modelVal ->
      (decode DisplayModel modelVal, Cmd.none)

port elmReceiver : (D.Value -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions _ =
  elmReceiver Receive

tabErrorView _ = text "active tab not found"

calendarView: DisplayModel -> Html Never
calendarView info =
  div [ class "flex-column" ]
    [ h1 [] [ text <| Date.toString info.date ]
    , text <| getTextField "teext" info
    , div [ class "flex-row", class "stretch-item" ]  
      ( Date.dayNames 
        |> List.indexedMap (\i day ->
           text (day) 
           |> U.aloneInside (h3 [])
           |> U.aloneInside (
              if Just i == Date.get "day" info.date
              then mark []
              else div []
           )
        ) 
      )
    , U.colTable
        [ [ text "Weather:", text "Location:" ]
        , [ text <| getTextField "weather" info , text <| getTextField "location" info ]
        ]
    ]

combatView: DisplayModel -> Html Never
combatView info =
  let
    getText i = 
      Arr.get i
      >> Maybe.withDefault ""
      >> text
    indexedGet str i = 
      getTextField (str ++ String.fromInt i) info
  in
    div [ class "flex-column" ]
      [ h2 [] [ text "Combat" ]
      , U.colTable (
          info.combatRows
          |> Arr.toList
          |> List.take info.nVisibleRows
          |> List.map (\row ->
             [ getText 0 row
             , getText 1 row
             ]
          )
          |> U.transpose
        )
      ]

displayView : Result D.Error DisplayModel -> Html Never
displayView model =
  case model of
    Err e ->
      div [] [ e |> D.errorToString |> text ]
    Ok info -> 
      U.get info.activeTab 
        [ calendarView
        , combatView
        ] |>
      Maybe.withDefault tabErrorView |>
      (|>) info
