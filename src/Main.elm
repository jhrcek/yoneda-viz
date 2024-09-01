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
    , editState : EditState
    }


type EditState
    = NotEditing
    | HoveringHomSet Int Int
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
    | CloseHomSetEditor


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

        SetEditState newSt ->
            let
                newEditState =
                    case ( model.editState, newSt ) of
                        ( NotEditing, _ ) ->
                            newSt

                        ( EditingHomSet _ _, EditingHomSet dId cId ) ->
                            EditingHomSet dId cId

                        ( EditingHomSet _ _, _ ) ->
                            -- When editing a hom set, we ignore enter/leave events and only alow exiting edit state by closing hom set editor
                            model.editState

                        ( HoveringHomSet _ _ , _ ) ->
                            newSt
            in
            ( { model | editState = newEditState }, Cmd.none )

        CloseHomSetEditor ->
            ( { model | editState = NotEditing }, Cmd.none )


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
        , Html.div [ A.class "m-controls" ]
            [ Html.table [ A.class "m-child" ]
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

                                                onMouseEnterHover =
                                                    E.onMouseEnter <| SetEditState <| HoveringHomSet domId codId

                                                onClickEditHomSet =
                                                    E.onClick <| SetEditState <| EditingHomSet domId codId

                                                cellCls =
                                                    A.class "m-cell"

                                                staticCell =
                                                    Html.td
                                                        [ cellCls
                                                        , onMouseEnterHover
                                                        , onClickEditHomSet
                                                        ]
                                                        [ if Set.isEmpty homSet then
                                                            if domId == codId then
                                                                idMorphism domLbl

                                                            else
                                                                Html.text "∅"

                                                          else
                                                            let
                                                                addIdMorphism =
                                                                    if domId == codId then
                                                                        (::) (idMorphism domLbl)

                                                                    else
                                                                        identity
                                                            in
                                                            Set.toList homSet
                                                                |> List.map morphismWithId
                                                                |> addIdMorphism
                                                                |> List.intersperse (Html.text ", ")
                                                                |> (\s -> Html.text "{" :: s ++ [ Html.text "}" ])
                                                                |> Html.span []
                                                        ]

                                                cell =
                                                    case editState of
                                                        HoveringHomSet dId cId ->
                                                            if dId == domId && cId == codId then
                                                                Html.td
                                                                    [ cellCls
                                                                    , E.onMouseLeave <| SetEditState NotEditing
                                                                    ]
                                                                    [ Html.text <|
                                                                        String.fromInt <|
                                                                            Set.size homSet
                                                                                + (if domId == codId then
                                                                                    -- identity morphism
                                                                                    1

                                                                                   else
                                                                                    0
                                                                                  )
                                                                    , Html.button
                                                                        [ E.onClick <| AddMorphism domId codId
                                                                        , A.title "Quick add morphism"
                                                                        ]
                                                                        [ Html.text "+" ]
                                                                    , Html.button
                                                                        [ onClickEditHomSet
                                                                        , A.title "Edit Hom Set"
                                                                        ]
                                                                        [ Html.text "🖉" ]
                                                                    ]

                                                            else
                                                                staticCell

                                                        EditingHomSet dId cId ->
                                                            if dId == domId && cId == codId then
                                                                Html.td
                                                                    [ cellCls
                                                                    , E.onClick CloseHomSetEditor
                                                                    ]
                                                                    [ Html.text "..." ]

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
            , -- Hom Set Editor
              case editState of
                EditingHomSet dId cId ->
                    let
                        idMorphItem =
                            Html.li [] [ idMorphism domLbl ]

                        domLbl =
                            Maybe.withDefault "?" <| Dict.get dId cat.objects

                        codLbl =
                            Maybe.withDefault "?" <| Dict.get cId cat.objects
                    in
                    Html.div [ A.class "m-child", A.class "m-editor" ]
                        [ Html.h4 [ A.style "margin-top" "0px" ] [ Html.text <| "Hom(" ++ domLbl ++ ", " ++ codLbl ++ ")" ]
                        , Html.button
                            [ A.class "close"
                            , E.onClick CloseHomSetEditor
                            ]
                            [ Html.text "🗙" ]
                        , Html.button [ E.onClick <| AddMorphism dId cId ]
                            [ Html.text "Add morphism" ]
                        , Html.div []
                            [ case Dict.get ( dId, cId ) homSets of
                                Just homSet ->
                                    let
                                        addIdMorphism =
                                            if dId == cId then
                                                (::) idMorphItem

                                            else
                                                identity
                                    in
                                    Set.toList homSet
                                        |> List.map
                                            (\morphId ->
                                                Html.li []
                                                    [ morphismWithId morphId
                                                    , Html.button [ E.onClick <| DeleteMorphism morphId ] [ Html.text "🗙" ]
                                                    ]
                                            )
                                        |> addIdMorphism
                                        |> Html.ul []

                                Nothing ->
                                    if dId == cId then
                                        Html.ul [] [ idMorphItem ]

                                    else
                                        Html.text "No morphisms between these objects."
                            ]
                        ]

                HoveringHomSet _ _ ->
                    Html.text ""

                NotEditing ->
                    Html.text ""
            ]
        ]


idMorphism : String -> Html msg
idMorphism domLbl =
    Html.span [] [ Html.text "id", Html.sub [] [ Html.text domLbl ] ]


morphismWithId : Int -> Html msg
morphismWithId morpId =
    Html.span [] [ Html.text "f", Html.sub [] [ Html.text (String.fromInt morpId) ] ]


headerCell : ( Int, String ) -> Html msg
headerCell ( _, lbl ) =
    Html.th [] [ Html.text lbl ]
