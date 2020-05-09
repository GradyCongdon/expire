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
    }


type Mode
    = Reading
    | Adding


type Msg
    = ItemAdd
    | ItemName String
    | ItemExpiration String
    | ModeAdding


type alias Model =
    { items : List Item
    , mode : Mode
    , itemName : String
    , itemExpiration : Int
    }


init : Model
init =
    { items =
        [ makeItem "melted onions"
        , makeItem2 "ham sandwich" 30
        ]
    , mode = Reading
    , itemName = ""
    , itemExpiration = 1
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ItemAdd ->
            let
                item =
                    { name = model.itemName
                    , expiration = model.itemExpiration
                    }
            in
            if model.itemName == "" then
                model

            else
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

        ModeAdding ->
            { model | mode = Adding }


view : Model -> Html Msg
view model =
    main_ [ class "main" ]
        [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
        , div [ class "container" ]
            [ div [ class "items" ] (renderItems model.items)
            , div [ class "controls" ] (renderControls model)
            ]
        ]


makeItem : String -> Item
makeItem name =
    { name = name
    , expiration = 3
    }


makeItem2 : String -> Int -> Item
makeItem2 name expiration =
    { name = name
    , expiration = expiration
    }


renderItems : List Item -> List (Html Msg)
renderItems items =
    items
        |> List.sortBy .expiration
        |> List.map
            (\i ->
                div [ class "item" ]
                    [ span [ class "expiration" ] [ text (String.fromInt i.expiration) ]
                    , span [ class "name" ] [ text i.name ]
                    ]
            )


renderControls : Model -> List (Html Msg)
renderControls model =
    let
        expiration =
            String.fromInt model.itemExpiration
    in
    case model.mode of
        Reading ->
            [ button [ type_ "submit", class "add", onClick ModeAdding ] [ text "+" ] ]

        Adding ->
            [ div [ class "left" ]
                [ input [ class "name", onInput ItemName ] []
                , renderExpiration expiration
                ]
            , button [ type_ "submit", class "add", onClick ItemAdd ] [ text "+" ]
            ]


renderExpiration : String -> Html Msg
renderExpiration expiration =
    div [ class "expiration-container" ]
        [ input
            [ type_ "range"
            , class "expiration"
            , Html.Attributes.min "1"
            , Html.Attributes.max "30"
            , onInput ItemExpiration
            , placeholder expiration
            , value expiration
            ]
            []
        , span [ class "expiration-value" ] [ text expiration ]
        ]
