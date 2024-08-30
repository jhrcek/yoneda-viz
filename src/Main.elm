module Main exposing (main)

import Browser
import Category as Cat exposing (Category)
import Dict
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Ports
import Set


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


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { cat = Cat.empty }
    in
    ( model, renderGraph model.cat )


renderGraph : Category -> Cmd msg
renderGraph cat =
    Ports.renderDot { engine = "dot", dotSource = Cat.renderDot cat }


type Msg
    = AddObject
    | DeleteObject Int
    | AddMorphism Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddObject ->
            updateCat Cat.addObject model

        DeleteObject objId ->
            -- TODO cap this at, say 10 objects
            updateCat (Cat.deleteObject objId) model

        AddMorphism domId codId ->
            -- TODO cap this at, say 10 morphisms per pair of objects
            updateCat (Cat.addMorphism domId codId) model


updateCat : (Category -> Category) -> Model -> ( Model, Cmd Msg )
updateCat f model =
    let
        newCat =
            f model.cat
    in
    ( { model | cat = newCat }
    , renderGraph newCat
    )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ A.id "top-container" ]
            [ Html.div [ A.id "rel-and-controls" ]
                [ viewObjectControls model.cat
                , viewHomSetTable model.cat
                ]
            , Html.div [ A.id "explanation" ]
                []
            , Html.div []
                [ Html.div [ A.id "graph" ]
                    [{- This is where viz.js renders svg graph -}]
                ]
            ]
        ]


viewObjectControls : Category -> Html Msg
viewObjectControls cat =
    Html.div []
        [ Html.h3 [] [ Html.text "Objects" ]
        , Html.button [ E.onClick AddObject ] [ Html.text "Add object" ]
        , cat.objects
            |> Dict.toList
            |> List.map
                (\( objId, lbl ) ->
                    Html.li []
                        [ Html.span [] [ Html.text lbl ] -- TODO make this editable
                        , Html.button [ E.onClick (DeleteObject objId) ] [ Html.text "🗙" ]
                        ]
                )
            |> Html.ul []
        ]


viewHomSetTable : Category -> Html Msg
viewHomSetTable cat =
    let
        objects =
            Dict.toList cat.objects

        homSets =
            Cat.getHomSets cat
    in
    Html.div [ A.id "rel" ]
        [ Html.h3 [] [ Html.text "Morphisms" ]
        , Html.table []
            [ Html.thead []
                [ Html.tr [] <|
                    Html.th [] [{- empty top-left corner -}]
                        :: List.map headerCell objects
                ]
            , Html.tbody [] <|
                List.map
                    (\( domId, domLbl ) ->
                        Html.tr [] <|
                            headerCell ( domId, domLbl )
                                :: List.map
                                    (\( codId, _ ) ->
                                        Html.td []
                                            [ Html.button [ E.onClick (AddMorphism domId codId) ] [ Html.text "+" ]
                                            , Html.text <|
                                                String.fromInt <|
                                                    (if domId == codId then
                                                        -- identity morphism
                                                        1

                                                     else
                                                        0
                                                    )
                                                        + Set.size (Maybe.withDefault Set.empty (Dict.get ( domId, codId ) homSets))
                                            , Html.button [] [ Html.text "-" ]
                                            ]
                                    )
                                    objects
                    )
                    objects
            ]
        ]


headerCell : ( Int, String ) -> Html msg
headerCell ( _, lbl ) =
    Html.th [] [ Html.text lbl ]
