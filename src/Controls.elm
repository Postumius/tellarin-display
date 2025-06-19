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
import Array as Arr exposing (Array)

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
  -- , backup : Backup
  , textFields : Dict Key String
  , activeTab : Tab
  , date : Date
  , combatRows : Array (Array String)
  , nVisibleRows : Int
  }

-- type Backup = Backup (List Model)

init : () -> ( Model, Cmd msg )
init _ =
  ( { activeTab = 0
    , textFields = 
      Dict.fromList
        [ ("teext", "")
        ]
    , date = Date.epoch
    , combatRows = Arr.empty
    , nVisibleRows = 0
    , focusedDenom = Nothing
    , denomBuffer = ""
    -- , backup = Backup []
    }
  , [("cmdString", E.string "load")]
    |> E.object
    |> elmSender
  )

type Msg 
  = SwitchTo Tab
  | GotTextFor Key String
  | ChangeDate (Date -> Date)
  | FocusDenom Int
  | BlurDenom (Date -> Date)
  | GotDenomInput String
  | ReceivedLoad D.Value
  | MainCommand String
  | EditCombatCell Int Int String
  | EditUltCell Int String
  | DeleteCombatRows Int
  | MoveCurtain (Int -> Int)

port elmSender : E.Value -> Cmd msg

port elmReceiver : (D.Value -> msg) -> Sub msg

subscriptions _ = elmReceiver ReceivedLoad


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
  let
    out encode m = (m, m |> encode |> elmSender)
    sendToDisplay = out (encodeWithCmd "send")
    noOp m = (m, Cmd.none)
    trimBlanks =
      U.dropBackWhile ((==) blankRow)
  in
    case msg of
      MainCommand cmdStr ->
        model
        |> out (encodeWithCmd cmdStr)
      ChangeDate f ->
        {model | date = model.date |> f} 
        |> sendToDisplay
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
      MoveCurtain f -> 
        { model 
        | nVisibleRows = 
          f model.nVisibleRows 
          |> Basics.min (Arr.length model.combatRows)
          |> Basics.max 0 
        }
        |> sendToDisplay
      EditCombatCell i j input -> 
        { model 
        | combatRows = 
          model.combatRows 
          |> U.tryGetSet (Arr.get i) (Arr.set i) (Arr.set j input)
          |> trimBlanks
        }
        |> sendToDisplay
      EditUltCell j input ->
        { model
        | combatRows = 
          model.combatRows
          |> Arr.push (Arr.set j input blankRow)
          |> trimBlanks
        }
        |> sendToDisplay
      DeleteCombatRows n -> 
        let 
          newLength = 
            model.combatRows
            |> Arr.length
            |> U.minus (Basics.max n 0)
            |> Basics.max 0
        in
          { model
          | combatRows = 
            model.combatRows 
            |> Arr.slice 0 newLength
          , nVisibleRows = Basics.min model.nVisibleRows newLength
          }
          |> sendToDisplay


blankRow = Arr.repeat 2 ""

encodeWithCmd : String -> Model -> E.Value
encodeWithCmd cmdString model = 
  E.object
    [ ("cmdString", E.string cmdString)
    , ("date", Date.encode model.date)
    , ("activeTab", E.int model.activeTab)
    , ("textFields", E.dict identity E.string model.textFields)
    , ("combatRows", E.array (E.array E.string) model.combatRows)
    , ("nVisibleRows", E.int model.nVisibleRows)
    ]

decode constructor =
  let
    dInfo =
      D.map5 constructor
        (D.field "textFields" <| D.dict D.string)
        (D.field "activeTab" D.int)
        (D.field "date" Date.decoder)
        (D.field "combatRows" <| D.array <| D.array D.string)
        (D.field "nVisibleRows" D.int)
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
  let 
    commandButtons cmdStrs =
      cmdStrs
      |> L.map (\cmdStr ->
         button [ onClick <| MainCommand cmdStr ] [ text cmdStr ]
      )
  in
    div [] 
      <| commandButtons 
         [ "save"
         , "load"
         , "show"
         , "hide"
         , "save and quit"
         ]
      ++ [ input 
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

textField name model = 
  input 
    [ placeholder <| "enter " ++ name
    , value <| getTextField name model
    , onInput <| GotTextFor name 
    ] []

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
    , div [] 
      [ text <| "Weather: "
      , textField "weather" model
      ]
    , div [] 
      [ text <| "Location: "
      , textField "location" model
      ]
    ]

combatView : Model -> Html Msg
combatView model =
  let 
    nRows = Arr.length model.combatRows
    combatCell isVisible sendMsg row default j =
      input 
        [ placeholder default
        , value <| (
            row
            |> Arr.get j
            |> Maybe.withDefault ""
          )
        , onInput <| sendMsg j
        , class <| if isVisible then "visible-row" else "hidden-row"
        ] []
    assembleRow cell =
      div []
        [ cell "Enemy Name" 0 
        , cell "Enemy AC" 1
        ]
  in
    div [] (
      [ h2 [] [ text "Combat" ]
      , div []
        [ button [ onClick (DeleteCombatRows 1) ] [ text "delete 1" ]
        , button 
          [ onClick (DeleteCombatRows nRows) ] 
          [ text "delete all" ]
        ]
      , div []
        [ button [ onClick (MoveCurtain U.inc) ] [ text "reveal 1" ]
        , button [ onClick (MoveCurtain U.dec) ] [ text "hide 1" ]
        , button 
          [ onClick (MoveCurtain <| U.minus model.nVisibleRows >> (+) nRows) ]
          [ text "reveal all" ]
        , button 
          [ onClick (MoveCurtain <| U.minus model.nVisibleRows) ]
          [ text "hide all" ]
        ]
      ] 
      ++ (
         model.combatRows
         |> Arr.indexedMap (\i row ->
            assembleRow
            <| combatCell (i < model.nVisibleRows) (EditCombatCell i) row
         )
         |> Arr.toList
      )
      ++ [ assembleRow <| combatCell False EditUltCell blankRow ]
    )

