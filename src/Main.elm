module Main exposing (main)

import Browser
import Category as Cat exposing (Category, Morphism(..))
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
    , -- TODO this should work across: graph, composition table, hom set table
      showIdentities : Bool
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
            , showIdentities = False
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
    | ToggleShowIdentities
    | UndefineComposition Int Int
    | DefineComposition Int Int Int
    | NoOp


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

        ToggleShowIdentities ->
            ( { model | showIdentities = not model.showIdentities }, Cmd.none )

        DefineComposition morpId1 morpId2 morpId ->
            updateCat (Cat.defineComposition morpId1 morpId2 morpId) model

        UndefineComposition morpId1 morpId2 ->
            updateCat (Cat.undefineComposition morpId1 morpId2) model

        NoOp ->
            ( model, Cmd.none )


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
                , viewCompositionTable model
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
        [ Html.h2 [] [ Html.text "Category" ]
        , Html.label []
            [ Html.input [ A.type_ "checkbox", E.onClick ToggleShowIdentities ] []

            -- TODO add info icon explaining what this does (hiding identities from composition table, hom sets and graph, but not from composition table results!)
            , Html.text " Show Identities"
            ]
        , Html.h3 [] [ Html.text "Objects" ]
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
    Html.div []
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


viewCompositionTable : Model -> Html Msg
viewCompositionTable { cat, showIdentities } =
    let
        morphs =
            Cat.listComposableMorphisms showIdentities cat

        getObjLabel objId =
            Maybe.withDefault "TODO???" <| Dict.get objId cat.objects

        morphHeaderCell mor =
            Html.th []
                [ case mor of
                    Identity objId ->
                        idMorphism (getObjLabel objId)

                    NonIdentity morphId _ ->
                        morphismWithId morphId
                ]

        homSets =
            Cat.getHomSets cat
    in
    Html.div []
        [ Html.h3 [] [ Html.text "Composition" ]
        , Html.table []
            [ Html.thead []
                [ Html.tr [] <|
                    Html.th [] [{- empty top-left corner -}]
                        :: List.map morphHeaderCell morphs.columns
                ]
            , Html.tbody [] <|
                List.map
                    (\mor1 ->
                        Html.tr [] <|
                            morphHeaderCell mor1
                                :: List.map
                                    (\mor2 ->
                                        let
                                            cell =
                                                case mor1 of
                                                    Identity objId1 ->
                                                        case mor2 of
                                                            Identity objId2 ->
                                                                if objId1 == objId2 then
                                                                    Html.td [] [ idMorphism <| Maybe.withDefault "TODO???" <| Dict.get objId1 cat.objects ]

                                                                else
                                                                    Html.td [] []

                                                            NonIdentity morphId2 nonIdM2 ->
                                                                if objId1 == nonIdM2.domId then
                                                                    Html.td [] [ morphismWithId morphId2 ]

                                                                else
                                                                    Html.td [] []

                                                    NonIdentity morphId1 nonIdM1 ->
                                                        case mor2 of
                                                            Identity objId2 ->
                                                                if nonIdM1.codId == objId2 then
                                                                    Html.td [] [ morphismWithId morphId1 ]

                                                                else
                                                                    Html.td [] []

                                                            NonIdentity morphId2 nonIdM2 ->
                                                                if nonIdM1.codId == nonIdM2.domId then
                                                                    let
                                                                        homSet =
                                                                            Maybe.withDefault Set.empty <| Dict.get ( nonIdM1.domId, nonIdM2.codId ) homSets

                                                                        composition =
                                                                            Dict.get ( morphId1, morphId2 ) cat.composition

                                                                        morphOption morphId =
                                                                            Html.option
                                                                                [ A.selected <| composition == Just morphId
                                                                                , A.value <| String.fromInt morphId
                                                                                ]
                                                                                [ morphismWithId morphId ]

                                                                        isIdentityAvailable =
                                                                            nonIdM1.domId == nonIdM2.codId
                                                                    in
                                                                    if Set.isEmpty homSet && not isIdentityAvailable then
                                                                        Html.td
                                                                            [ A.title <|
                                                                                "hom("
                                                                                    ++ getObjLabel nonIdM1.domId
                                                                                    ++ ", "
                                                                                    ++ getObjLabel nonIdM2.codId
                                                                                    ++ ") = ∅\nThere are no morphisms from "
                                                                                    ++ getObjLabel nonIdM1.domId
                                                                                    ++ " to "
                                                                                    ++ getObjLabel nonIdM2.codId
                                                                                    ++ ".\nAdd at least one!"
                                                                            ]
                                                                            [ Html.text "⚠" ]

                                                                    else
                                                                        let
                                                                            unfedOption =
                                                                                Html.option [ A.selected <| composition == Nothing ] [ Html.text "?" ]

                                                                            addIdentity =
                                                                                if isIdentityAvailable then
                                                                                    (::)
                                                                                        (Html.option
                                                                                            [ A.selected <| composition == Just (negate nonIdM1.domId)
                                                                                            , A.value <| String.fromInt (negate nonIdM1.domId)
                                                                                            ]
                                                                                            [ idMorphism (getObjLabel nonIdM1.domId) ]
                                                                                        )

                                                                                else
                                                                                    identity
                                                                        in
                                                                        Html.td []
                                                                            [ Html.select
                                                                                [ E.onInput <|
                                                                                    \str ->
                                                                                        case str of
                                                                                            "?" ->
                                                                                                UndefineComposition morphId1 morphId2

                                                                                            s ->
                                                                                                case String.toInt s of
                                                                                                    Just morphId ->
                                                                                                        DefineComposition morphId1 morphId2 morphId

                                                                                                    Nothing ->
                                                                                                        NoOp
                                                                                ]
                                                                              <|
                                                                                unfedOption
                                                                                    :: addIdentity (List.map morphOption (Set.toList homSet))
                                                                            ]

                                                                else
                                                                    Html.td [] []
                                        in
                                        cell
                                    )
                                    morphs.columns
                    )
                    morphs.rows
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
