module Main exposing (Item, Model, init, main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Item =
    { name : String
    , expiration : Int
    , amount : Int
    }


type Mode
    = Reading
    | Adding


type Msg
    = ItemAdd
    | ItemName String
    | ItemExpiration String
    | ItemAmount String
    | ModeAdding


type alias Model =
    { items : List Item
    , mode : Mode
    , itemName : String
    , itemExpiration : Int
    , itemAmount : Int
    }


init : Model
init =
    { items = [ makeItem "melted onions" ]
    , mode = Reading
    , itemName = ""
    , itemExpiration = 1
    , itemAmount = 100
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ItemAdd ->
            let
                item =
                    makeItem model.itemName
            in
            { model
                | items = item :: model.items
                , itemName = ""
                , mode = Reading
            }

        ItemName name ->
            { model | itemName = name }

        ItemExpiration expiration ->
            case String.toInt expiration of
                Just number ->
                    { model | itemExpiration = number }

                Nothing ->
                    model

        ItemAmount amount ->
            case String.toInt amount of
                Just number ->
                    { model | itemAmount = number }

                Nothing ->
                    model

        ModeAdding ->
            { model | mode = Adding }


view : Model -> Html Msg
view model =
    main_ [ class "main" ]
        [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
        , div [ class "container" ]
            [ renderItems model.items
            , renderControls model |> div [ class "controls" ]
            ]
        ]


makeItem : String -> Item
makeItem name =
    { name = name
    , expiration = 3
    , amount = 100
    }


renderItems : List Item -> Html Msg
renderItems items =
    items
        |> List.map
            (\i ->
                div [ class "item" ]
                    [ span [ class "name" ] [ text i.name ]
                    , span [ class "expiration" ] [ text (String.fromInt i.expiration) ]
                    , span [ class "amount" ] [ text (String.fromInt i.amount) ]
                    ]
            )
        |> div [ class "items" ]


renderControls : Model -> List (Html Msg)
renderControls model =
    case model.mode of
        Reading ->
            [ button [ type_ "submit", onClick ModeAdding ] [ text "+" ] ]

        Adding ->
            [ input [ class "name", onInput ItemName, value model.itemName ] []
            , input [ class "expiration", onInput ItemExpiration, value (String.fromInt model.itemExpiration) ] []
            , input [ class "amount", onInput ItemAmount, value (String.fromInt model.itemAmount) ] []
            , button [ type_ "submit", onClick ItemAdd ] [ text "add" ]
            ]
