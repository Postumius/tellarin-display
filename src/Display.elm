port module Display exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Json.Decode as D
import Maybe
import Dict exposing (Dict)

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
    , [ [ h2[] [text "Weather: "], h2[] [text <| getTextField "weather" info ]]
      , [ h2[] [text "Location: "], h2[] [text <| getTextField "location" info ]]
      ]
      |> U.transpose
      |> List.map (div [ class "flex-column" ])
      |> div [ class "flex-row" ]
    ]

combatView: DisplayModel -> Html Never
combatView info =
  let
    indexedGet str i = 
      getTextField (str ++ String.fromInt i) info
  in
    div [ class "flex-column" ]
      [ h2 [] [ text "Combat" ]
      , U.table 
        { headerWrap = h2 []
        , bodyWrap = h3 []
        , attributes = []
        , rows = 
          [ text "Character"
          , text "AC"
          ]
          :: (
             List.range 1 info.nCombatRows |> List.map (\i ->
               [ text <| indexedGet "name" i
               , text <| indexedGet "AC" i
               ]
             )
          )
        }
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
