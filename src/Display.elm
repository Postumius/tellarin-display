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

adventureView: DisplayModel -> Html Never
adventureView info =
  div [ class "flex-column" ]
    [ h2 [] [ text <| Date.toString info.date ]
    , text <| getTextField "teext" info
    , Date.dayNames 
      |> List.indexedMap (\i day ->
         text (day) 
         |> U.aloneInside (
            if Just i == Date.get "day" info.date
            then mark []
            else div []
         )
      ) 
      |> div [ class "flex-row", class "stretch-item" ]  
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
    h2 [] [ text "Combat" ]
    :: rows
    |> div [ class "flex-column" ]

displayView : Result D.Error DisplayModel -> Html Never
displayView model =
  case model of
    Err e ->
      div [] [ e |> D.errorToString |> text ]
    Ok info -> 
      U.get info.activeTab 
        [ adventureView
        , combatView
        ] |>
      Maybe.withDefault tabErrorView |>
      (|>) info
