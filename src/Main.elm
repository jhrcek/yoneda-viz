module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Ports


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { cat : Category
    }


type alias Category =
    { objectCount : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { cat = { objectCount = 4 } }
    in
    ( model, renderGraph model.cat )


renderGraph : Category -> Cmd msg
renderGraph cat =
    Ports.renderDot { engine = "dot", dotSource = renderDot cat }


renderDot : Category -> String
renderDot cat =
    let
        nodeLines =
            List.range 1 cat.objectCount |> List.map String.fromInt

        edgeLines =
            -- TODO
            []
    in
    String.join ";" <|
        "digraph G{graph[rankdir=BT;splines=true;overlap=false]"
            :: "node[shape=circle;width=0.3;fixedsize=true]"
            :: (nodeLines ++ edgeLines)
            ++ [ "}" ]


type Msg
    = SetObjectCount Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetObjectCount newCount ->
            updateCat (setObjectCount newCount) model


updateCat : (Category -> Category) -> Model -> ( Model, Cmd Msg )
updateCat f model =
    let
        newCat =
            f model.cat
    in
    ( { model | cat = newCat }
    , renderGraph newCat
    )


setObjectCount : Int -> Category -> Category
setObjectCount newCount cat =
    { cat | objectCount = newCount }



-- pure : a -> ( a, Cmd msg )
-- pure a =
--     ( a, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ A.id "top-container" ]
            [ Html.div [ A.id "rel-and-controls" ]
                [ objCountInputView model.cat.objectCount
                ]
            , Html.div [ A.id "explanation" ]
                [ Html.text "TODO" ]
            , Html.div []
                [ Html.div [ A.id "graph" ]
                    [{- This is where viz.js renders svg graph -}]
                ]
            ]
        ]


objCountInputView : Int -> Html Msg
objCountInputView objectCount =
    Html.div []
        [ Html.label []
            [ Html.span [ A.id "size-label" ]
                [ Html.text <| "Object count = " ++ String.fromInt objectCount ]
            , Html.input
                [ A.type_ "range"
                , A.min "1"
                , A.max "10"
                , E.onInput <| SetObjectCount << Maybe.withDefault 3 << String.toInt
                , A.value <| String.fromInt objectCount
                ]
                []
            ]
        ]
