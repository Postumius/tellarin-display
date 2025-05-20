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
  }

init : () -> ( Model, Cmd msg )
init _ =
  ( { activeTab = 0
    , textFields = Dict.singleton "teext" ""
    , date = Date.epoch
    , focusedDenom = Nothing
    , denomBuffer = ""
    }
  , Cmd.none)

type Msg 
  = SwitchTo Tab
  | GotTextFor Key String
  | StepDate Date.DenomModifier Int
  | SetDate (Date -> Date)
  | FocusDenom Int
  | BlurDenom (Date -> Date)
  | GotDenomInput String
  | Save
  | RequestLoad
  | ReceivedLoad D.Value

port elmSender : E.Value -> Cmd msg

port elmReceiver : (D.Value -> msg) -> Sub msg

subscriptions _ = elmReceiver ReceivedLoad



update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
  let
    out encode m = (m, m |> encode |> elmSender)
    send = out (encodeWithCmd "send")
    save = out (encodeWithCmd "save")
    noOp m = (m, Cmd.none)
  in
    case msg of
      StepDate modifier n ->
        {model | date = model.date |> modifier ((+) n)} 
        |> send
      SetDate setVal ->
        {model | date = model.date |> setVal} 
        |> send
      Save ->
        model 
        |> save
      RequestLoad -> 
        model
        |> out (always <| E.object [("cmdString", E.string "load")])
      ReceivedLoad json -> 
        model
        |> mergeLoaded json
        |> send
      SwitchTo tab ->
        {model | activeTab=tab} 
        |> send
      GotTextFor key str ->
        updateTextFields (Dict.insert key str) model
        |> send
      FocusDenom i ->
        { model | focusedDenom = Just i }
        |> updateTextFields (Dict.insert "denomBuffer" "")
        |> noOp
      BlurDenom setVal ->
        { model
        | focusedDenom = Nothing
        , date = model.date |> setVal
        } 
        |> send
      GotDenomInput str ->
        { model | denomBuffer = str } 
        |> noOp

encodeWithCmd : String -> Model -> E.Value
encodeWithCmd cmdString model = 
  E.object
    [ ("cmdString", E.string cmdString)
    , ("date", Date.encode model.date)
    , ("activeTab", E.int model.activeTab)
    , ("textFields", E.dict identity E.string model.textFields)
    ]

decode constructor =
  let
    dInfo =
      D.map3 constructor
        (D.field "textFields" <| D.dict D.string)
        (D.field "activeTab" D.int)
        (D.field "date" Date.decoder)
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
  |> Maybe.withDefault "key not found"



view : Model -> Html Msg
view model = 
  makeTabView model
    [ ("Now", nowView)
    , ("ACs", always <| h2 [] [ text "ACs" ])
    ]

denomInputView : Int -> Date.TimeDenomination -> Model -> Html Msg
denomInputView i denom model =
  let 
    denomVal = model.date |> denom.getter
  in
    input
      -- [ value <| String.fromInt n
      -- , onInput (String.toInt >> Maybe.withDefault n >> denom.setter >> SetDate)
      -- ]
      [ placeholder <| String.fromInt denomVal
      , onFocus <| FocusDenom i
      , value <| if Just i == model.focusedDenom then model.denomBuffer else ""
      , onInput <| GotDenomInput
      , onBlur <| BlurDenom (
          model.denomBuffer 
          |> String.toInt 
          |> Maybe.withDefault denomVal 
          |> denom.setter
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
      |> L.singleton 
      |> div []
  in
    div [] [ selector, tabBody]

nowView : Model -> Html Msg
nowView model =
  div []
    [ h2 [] [ text "Now" ]
    , button [ onClick Save ] [ text "save" ]
    , button [ onClick RequestLoad ] [ text "load" ]
    , input 
        [ placeholder "enter teext"
        , value <| getTextField "teext" model
        , onInput <| GotTextFor "teext" 
        ] []
    , Date.denoms 
      |> NE.toList 
      |> L.indexedMap (\i denom ->
        [ button 
          [ onClick (StepDate denom.modifier -1)
          , class "button" 
          ] [ "-1 "++denom.name |> text]
        , denomInputView i denom model
        , button 
            [ onClick (StepDate denom.modifier 1) 
            , class "button"
            ] [ "+1 "++denom.name |> text ]
        , case denom.base of
            Just (Date.Names names) -> 
              radioButtons (denom.getter model.date) (denom.setter >> SetDate) names 
              |> div []
            _ -> div [] [text "placeholder"]
        ]
      )
      |> U.transpose 
      |> L.map (div [class "flex-column"]) 
      |> div [class "flex-row"]
    ]
