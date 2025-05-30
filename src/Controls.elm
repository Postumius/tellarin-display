port module Controls exposing 
  ( decode
  , Key
  , updateTextFields
  , getTextField
  )

import Browser
import Html as H exposing (..)-- (Html, button, div, text)
import Html.Attributes exposing (..) 
import Html.Events exposing (..)
import Json.Encode as E
import Json.Decode as D
import Dict exposing (Dict)
import List as L 

import Utilities as U
import TellarinDate as Date exposing (Date)
import NonEmpty as NE exposing (NonEmpty(..))

main =
  Browser.element 
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

type alias Tab = Int
type alias Key = String

type alias Model =
  { focusedDenom : Maybe Int
  , denomBuffer : String
  , textFields : Dict Key String
  , activeTab : Tab
  , date : Date
  , nCombatRows : Int
  }

init : () -> ( Model, Cmd msg )
init _ =
  ( { activeTab = 0
    , textFields = 
      Dict.fromList
        [ ("teext", "")
        ]
    , date = Date.epoch
    , nCombatRows = 0
    , focusedDenom = Nothing
    , denomBuffer = ""
    }
  , Cmd.none)

type Msg 
  = SwitchTo Tab
  | GotTextFor Key String
  | ChangeDate (Date -> Date)
  | FocusDenom Int
  | BlurDenom (Date -> Date)
  | GotDenomInput String
  | Save
  | RequestLoad
  | ReceivedLoad D.Value
  | ChangeNRows (Int -> Int)
  | Show
  | Hide

port elmSender : E.Value -> Cmd msg

port elmReceiver : (D.Value -> msg) -> Sub msg

subscriptions _ = elmReceiver ReceivedLoad



update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
  let
    out encode m = (m, m |> encode |> elmSender)
    sendToDisplay = out (encodeWithCmd "send")
    noOp m = (m, Cmd.none)
    command str = 
      [("cmdString", E.string str)]
      |> E.object
      |> elmSender
  in
    case msg of
      ChangeDate f ->
        {model | date = model.date |> f} 
        |> sendToDisplay
      Save ->
        model 
        |> out (encodeWithCmd "save")
      RequestLoad -> 
        ( model
        , command "load"
        )
      ReceivedLoad json -> 
        model
        |> mergeLoaded json
        |> sendToDisplay
      SwitchTo tab ->
        {model | activeTab=tab} 
        |> sendToDisplay
      GotTextFor key str ->
        updateTextFields (Dict.insert key str) model
        |> sendToDisplay
      FocusDenom i ->
        { model | focusedDenom = Just i }
        |> updateTextFields (Dict.insert "denomBuffer" "")
        |> noOp
      BlurDenom setVal ->
        { model
        | focusedDenom = Nothing
        , date = model.date |> setVal
        } 
        |> sendToDisplay
      GotDenomInput str ->
        { model | denomBuffer = str } 
        |> noOp
      ChangeNRows f -> 
        { model | nCombatRows = Basics.max 0 <| f model.nCombatRows }
        |> sendToDisplay
      Show ->
        (model, command "show")
      Hide ->
        (model, command "hide")


encodeWithCmd : String -> Model -> E.Value
encodeWithCmd cmdString model = 
  E.object
    [ ("cmdString", E.string cmdString)
    , ("date", Date.encode model.date)
    , ("activeTab", E.int model.activeTab)
    , ("textFields", E.dict identity E.string model.textFields)
    , ("nCombatRows", E.int model.nCombatRows)
    ]

decode constructor =
  let
    dInfo =
      D.map4 constructor
        (D.field "textFields" <| D.dict D.string)
        (D.field "activeTab" D.int)
        (D.field "date" Date.decoder)
        (D.field "nCombatRows" D.int)
  in
    D.decodeValue dInfo

mergeLoaded json model =
  case decode (Model model.focusedDenom model.denomBuffer) json of
    Err e -> 
      model
      |> updateTextFields (Dict.insert "teext" <| D.errorToString e)
    Ok m -> m

updateTextFields f model =
  { model | textFields = f model.textFields }

getTextField : Key -> {a | textFields: Dict Key String} -> String
getTextField key model =
  model.textFields
  |> Dict.get key 
  |> Maybe.withDefault ""



view : Model -> Html Msg
view model = 
  div [] 
    [ button [ onClick Save ] [ text "save" ]
    , button [ onClick RequestLoad ] [ text "load" ]
    , button [ onClick Show ] [ text "show" ]
    , button [ onClick Hide ] [ text "hide" ]
    , input 
        [ placeholder "enter teext"
        , value <| getTextField "teext" model
        , onInput <| GotTextFor "teext" 
        ] []
    , hr [] [] 
    , makeTabView model
      [ ("Calendar", calenderView)
      , ("Combat", combatView)
      ]
    ]

denomInputView : Int -> Date.TimeDenomination -> Model -> Html Msg
denomInputView i denom model =
  let 
    denomVal = model.date |> Date.startGetter denom
  in
    input
      [ placeholder <| String.fromInt denomVal
      , onFocus <| FocusDenom i
      , value <| if Just i == model.focusedDenom then model.denomBuffer else ""
      , onInput <| GotDenomInput
      , onBlur <| BlurDenom (
          model.denomBuffer 
          |> String.toInt 
          |> Maybe.withDefault denomVal 
          |> Date.startSetter denom
        )
      ] []

radioButtons : Int -> (Int -> Msg) -> List String -> List (Html Msg)
radioButtons selectedIndex sendSelection optionNames =
  optionNames 
  |> L.indexedMap (\i name ->
    button 
      [ onClick <| sendSelection i
      , disabled <| i == selectedIndex 
      ] [ text name ]
  )

makeTabView : Model -> List (String, Model -> Html Msg) -> Html Msg
makeTabView model tabViews =
  let
    selector =
      tabViews 
      |> L.map Tuple.first
      |> radioButtons model.activeTab SwitchTo 
      |> div []
    tabBody =
      tabViews
      |> U.get model.activeTab 
      |> Maybe.map Tuple.second 
      |> Maybe.withDefault (always <| text "active tab not found") 
      |> (|>) model 
      |> U.aloneInside (div [])
  in
    div [] [ selector, tabBody]

calenderView : Model -> Html Msg
calenderView model =
  div []
    [ h2 [] [ text <| Date.toString model.date ]
    , Date.denoms 
      |> NE.toList 
      |> L.indexedMap (\i denom ->
         [ button 
           [ onClick (ChangeDate <| denom.modifier U.dec)
           , class "button" 
           ] [ "-1 "++denom.name |> text]
         , button 
             [ onClick (ChangeDate <| denom.modifier U.inc) 
             , class "button"
             ] [ "+1 "++denom.name |> text ]
         , case denom.base of
             Just (Date.Names names) -> 
               radioButtons 
                 (model.date |> Date.startGetter denom |> U.dec)
                 (U.inc >> Date.startSetter denom >> ChangeDate)
                 names 
               |> div []
             _ -> denomInputView i denom model
         ]
      )
      |> U.transpose 
      |> L.map (div [class "flex-column"]) 
      |> div [class "flex-row"]
    ]

combatRow model i=
  let 
    suffix str = str ++ String.fromInt i
    name = suffix "name"
    ac = suffix "AC"
  in
    div []
      [ input 
          [ placeholder "Enemy Name"
          , value <| getTextField name model
          , onInput <| GotTextFor name
          ] []
        , input 
            [ placeholder "Enemy AC"
            , value <| getTextField ac model
            , onInput <| GotTextFor ac
            ] []
      ]

combatView : Model -> Html Msg
combatView model =
  [ h2 [] [ text "Combat" ]
  , button [ onClick (ChangeNRows U.inc) ] [ text "add row" ]
  , button [ onClick (ChangeNRows U.dec) ] [ text "remove row" ]
  ] 
  ++ (L.range 1 model.nCombatRows |> L.map (combatRow model))
  |> div []
