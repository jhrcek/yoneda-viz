module Main exposing (main)

import Browser
import Category as Cat exposing (Category)
import Dict
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
    , editState : EditState
    }


type EditState
    = NotEditing
    | EditingHomSet Int Int


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { cat = Cat.empty
            , editState = NotEditing
            }
    in
    ( model, renderGraph model.cat )


renderGraph : Category -> Cmd msg
renderGraph cat =
    Ports.renderDot { engine = "dot", dotSource = Cat.renderDot cat }


type Msg
    = AddObject
    | DeleteObject Int
    | AddMorphism Int Int
    | DeleteMorphism Int
    | SetEditState EditState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddObject ->
            updateCat Cat.addObject model

        DeleteObject objId ->
            -- TODO cap this at, say 10 objects
            updateCat (Cat.deleteObject objId) model

        DeleteMorphism morpId ->
            updateCat (Cat.deleteMorphism morpId) model

        AddMorphism domId codId ->
            -- TODO cap this at, say 10 morphisms per pair of objects
            updateCat (Cat.addMorphism domId codId) model

        SetEditState newState ->
            ( { model | editState = newState }, Cmd.none )


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
            [ Html.div [ A.id "controls" ]
                [ viewObjectControls model.cat
                , viewHomSetTable model
                ]
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


viewHomSetTable : Model -> Html Msg
viewHomSetTable { cat, editState } =
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
                                        let
                                            homSet =
                                                Maybe.withDefault Set.empty <| Dict.get ( domId, codId ) homSets

                                            cellCls =
                                                A.class "m-cell"

                                            addIdMorphism =
                                                if domId == codId then
                                                    (::) (idMorphism domLbl)

                                                else
                                                    identity

                                            viewHomSet : (Int -> Html msg) -> Set Int -> Html msg
                                            viewHomSet renderMorphism set =
                                                Set.toList set
                                                    |> List.map renderMorphism
                                                    |> addIdMorphism
                                                    |> List.intersperse (Html.text ", ")
                                                    |> (\items ->
                                                            if List.isEmpty items then
                                                                [ Html.text "∅" ]

                                                            else
                                                                Html.text "{" :: items ++ [ Html.text "}" ]
                                                       )
                                                    |> Html.span []

                                            staticCell =
                                                Html.td
                                                    [ cellCls
                                                    , E.onMouseEnter <| SetEditState <| EditingHomSet domId codId
                                                    ]
                                                    [ viewHomSet morphismWithId homSet ]

                                            cell =
                                                case editState of
                                                    EditingHomSet dId cId ->
                                                        if dId == domId && cId == codId then
                                                            Html.td
                                                                [ cellCls
                                                                , A.class "m-edit"
                                                                , E.onMouseLeave <| SetEditState NotEditing
                                                                ]
                                                                [ viewHomSet
                                                                    (\morphId ->
                                                                        Html.span []
                                                                            [ morphismWithId morphId
                                                                            , Html.button
                                                                                [ E.onClick <| DeleteMorphism morphId
                                                                                , A.title "Remove morphism"
                                                                                ]
                                                                                [ Html.text "🗙" ]
                                                                            ]
                                                                    )
                                                                    homSet
                                                                , Html.button
                                                                    [ A.class "m-add"
                                                                    , E.onClick <| AddMorphism dId cId
                                                                    , A.title "Add morphism"
                                                                    ]
                                                                    [ Html.text "+" ]
                                                                ]

                                                        else
                                                            staticCell

                                                    NotEditing ->
                                                        staticCell
                                        in
                                        cell
                                    )
                                    objects
                    )
                    objects
            ]
        ]


idMorphism : String -> Html msg
idMorphism domLbl =
    morphism "id" domLbl


morphismWithId : Int -> Html msg
morphismWithId morphId =
    morphism "f" (String.fromInt morphId)


morphism : String -> String -> Html msg
morphism name sub =
    Html.span [] [ Html.text name, Html.sub [] [ Html.text sub ] ]


headerCell : ( Int, String ) -> Html msg
headerCell ( _, lbl ) =
    Html.th [] [ Html.text lbl ]
