module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Ports
import Set exposing (Set)


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
    , morphisms : Dict ( Int, Int ) (Set Int) -- (domId, codId) -> Set morphismId
    , morphismIdGen : Int
    }


addMorphism : Int -> Int -> Category -> Category
addMorphism domId codId cat =
    { cat
        | morphisms =
            Dict.update ( domId, codId )
                (\morpSet ->
                    Just
                        (case morpSet of
                            Nothing ->
                                Set.singleton cat.morphismIdGen

                            Just ms ->
                                Set.insert cat.morphismIdGen ms
                        )
                )
                cat.morphisms
        , morphismIdGen = cat.morphismIdGen + 1
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { cat =
                { objectCount = 4
                , morphisms = Dict.empty
                , morphismIdGen = 0
                }
            }
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
            cat.morphisms
                |> Dict.foldl
                    (\( domId, codId ) morphs acc ->
                        Set.foldl
                            (\morpId acc2 ->
                                (String.fromInt domId ++ "->" ++ String.fromInt codId ++ "[label=" ++ String.fromInt morpId ++ "]")
                                    :: acc2
                            )
                            acc
                            morphs
                    )
                    []
    in
    String.join ";" <|
        "digraph G{graph[rankdir=BT;splines=true;overlap=false]"
            :: "node[shape=circle;width=0.3;fixedsize=true]"
            :: (nodeLines ++ edgeLines)
            ++ [ "}" ]


type Msg
    = SetObjectCount Int
    | AddMorphism Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetObjectCount newCount ->
            -- TODO remove all objects/morphisms that no longer fit
            updateCat (setObjectCount newCount) model

        AddMorphism domId codId ->
            -- TODO cap this at, say 10 morphisms per pair of objects
            updateCat (addMorphism domId codId) model


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
                , viewObjects model.cat
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


viewObjects : Category -> Html Msg
viewObjects cat =
    let
        objects =
            List.range 1 cat.objectCount
    in
    Html.div [ A.id "rel" ]
        [ Html.table []
            [ Html.thead []
                [ Html.tr [] <|
                    Html.th [] [{- empty top-left corner -}]
                        :: List.map headerCell objects
                ]
            , Html.tbody [] <|
                List.map
                    (\rowIdx ->
                        Html.tr [] <|
                            headerCell rowIdx
                                :: List.map
                                    (\colIdx ->
                                        Html.td []
                                            [ Html.button [ E.onClick (AddMorphism rowIdx colIdx) ] [ Html.text "+" ]
                                            , Html.text <|
                                                String.fromInt <|
                                                    (if rowIdx == colIdx then
                                                        -- identity morphism
                                                        1

                                                     else
                                                        0
                                                    )
                                                        + Set.size (Maybe.withDefault Set.empty (Dict.get ( rowIdx, colIdx ) cat.morphisms))
                                            , Html.button [] [ Html.text "-" ]
                                            ]
                                    )
                                    objects
                    )
                    objects
            ]
        ]


headerCell : Int -> Html msg
headerCell i =
    Html.th [] [ Html.text (String.fromInt i) ]
