module Main exposing (..)
import MergeSort
import Stack
import Browser
import Html exposing (Html, Attribute, div, input, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

-- MODEL

type alias Model =
  { item : String,
    array : List Int,
    sortedArray : List Int,
    hist : List (List ( List Int))
  }


init : Model
init =
  { item = "" , array = [] , sortedArray = []  , hist = [] }


type Msg
  = GetItem String | AddToList | Sort


-- UPDATE

update : Msg -> Model -> Model
update msg model =
  case msg of
    GetItem newItem->
      { model | item = newItem }  
    AddToList ->
      { model | array= model.array ++ [ String.toInt model.item|> Maybe.withDefault 0] }
    Sort ->
      let sa = MergeSort.mergeSort model.array
      in
        { model | sortedArray = Tuple.first sa , hist = Tuple.second sa }


-- VIEW
view : Model -> Html Msg
view model =
  let
    itemStrings = List.map String.fromInt model.array |> String.join "" 
  in
    div []
    [ text ("Add item to list :   " ),
      input [  id "item",value model.item, onInput GetItem ] [],
      text ("    " ),
      button [onClick AddToList] [ text "Add" ],
      div []
        [ text ("Items to sort    " ),
          text (List.map String.fromInt model.array |> String.join "    ")      
        ],
        div []
        [
          button [onClick Sort] [ text "Merge Sort" ] 
        ],
        div []
        [ text ("Sorted  :" ),
          text (List.map String.fromInt model.sortedArray |> String.join "    ")      
        ],
        div []
        [ text ("Path  :" ),
          text (List.foldl (\x a ->a ++ (Debug.toString x) ++ "====>" ) "" model.hist),
          text (Debug.toString model.sortedArray)      
        ]
    ] 


-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }
