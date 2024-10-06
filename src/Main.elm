module Main exposing (main)

import Browser
import Category as Cat exposing (Category, Morphism(..), NotAssociativeWitness, ViewConfig)
import Dict
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Ports
import Set exposing (Set)
import SolidColor as Color


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
    , viewConfig : ViewConfig
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
            , viewConfig = viewConfig
            }

        viewConfig =
            { showIdentities = False
            , colorObjects = True
            }
    in
    ( model, renderGraph viewConfig model.cat )


renderGraph : ViewConfig -> Category -> Cmd msg
renderGraph viewConfig cat =
    Ports.renderDot
        { engine = "dot"
        , dotSource = Cat.renderDotString viewConfig cat
        }


type Msg
    = AddObject
    | DeleteObject Int
    | AddMorphism Int Int
    | DeleteMorphism Int
    | SetEditState EditState
    | ToggleShowIdentities
    | ToggleColorObjects
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

        DeleteMorphism morphId ->
            updateCat (Cat.deleteMorphism morphId) model

        AddMorphism domId codId ->
            -- TODO cap this at, say 10 morphisms per pair of objects
            updateCat (Cat.addMorphism domId codId) model

        SetEditState newState ->
            ( { model | editState = newState }, Cmd.none )

        ToggleShowIdentities ->
            updateViewConfig Cat.toggleShowIdentities model

        ToggleColorObjects ->
            updateViewConfig Cat.toggleColorObjects model

        DefineComposition morphId1 morphId2 morphId ->
            updateCat (Cat.defineComposition morphId1 morphId2 morphId) model

        UndefineComposition morphId1 morphId2 ->
            updateCat (Cat.undefineComposition morphId1 morphId2) model

        NoOp ->
            ( model, Cmd.none )


updateViewConfig : (ViewConfig -> ViewConfig) -> Model -> ( Model, Cmd Msg )
updateViewConfig f model =
    let
        newViewConfig =
            f model.viewConfig
    in
    ( { model | viewConfig = newViewConfig }
    , renderGraph newViewConfig model.cat
    )


updateCat : (Category -> Category) -> Model -> ( Model, Cmd Msg )
updateCat f model =
    let
        newCat =
            f model.cat
    in
    ( { model | cat = newCat }
    , renderGraph model.viewConfig newCat
    )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ A.id "top-container" ]
            [ Html.div [ A.id "controls" ]
                [ viewObjectControls model
                , viewHomSetTable model
                , viewCompositionTable model
                ]
            , Html.div []
                [ Html.div [ A.id "graph" ]
                    [{- This is where viz.js renders svg graph -}]
                ]
            ]
        ]


viewObjectControls : Model -> Html Msg
viewObjectControls { cat, viewConfig } =
    Html.div []
        [ Html.h2 [] [ Html.text "Category" ]
        , Html.label []
            [ Html.input
                [ A.type_ "checkbox"
                , E.onClick ToggleShowIdentities
                , A.checked viewConfig.showIdentities
                , A.id "show-ids"
                ]
                []

            -- TODO add info icon explaining what this does (hiding identities from composition table, hom sets and graph, but not from composition table results!)
            , Html.text " Show Identities"
            ]
        , Html.label []
            [ Html.input
                [ A.type_ "checkbox"
                , E.onClick ToggleColorObjects
                , A.checked viewConfig.colorObjects
                , A.id "color-objs"
                ]
                []
            , Html.text " Color Objects"
            ]
        , Html.h3 [] [ Html.text "Objects" ]
        , Html.button [ E.onClick AddObject ] [ Html.text "Add object" ]
        , cat.objects
            |> Dict.toList
            |> List.map
                (\( objId, obj ) ->
                    Html.li []
                        [ Html.span [] [ Html.text obj.label ] -- TODO make this editable
                        , Html.button [ E.onClick (DeleteObject objId) ] [ Html.text "🗙" ]
                        ]
                )
            |> Html.ul []
        ]


viewHomSetTable : Model -> Html Msg
viewHomSetTable { cat, editState, viewConfig } =
    let
        objects =
            Dict.toList cat.objects

        content =
            if List.isEmpty objects then
                Html.div []
                    [ Html.text "There are no objects between which morphisms can be defined. Add at least one object!" ]

            else
                let
                    homSets =
                        Cat.getHomSets cat

                    headerCell =
                        objHeaderCell viewConfig.colorObjects
                in
                Html.table []
                    [ Html.thead []
                        [ Html.tr [] <|
                            Html.th [] [{- empty top-left corner -}]
                                :: List.map headerCell objects
                        ]
                    , Html.tbody [] <|
                        List.map
                            (\( domId, domObj ) ->
                                Html.tr [] <|
                                    headerCell ( domId, domObj )
                                        :: List.map
                                            (\( codId, _ ) ->
                                                let
                                                    homSet =
                                                        Maybe.withDefault Set.empty <| Dict.get ( domId, codId ) homSets

                                                    cellCls =
                                                        A.class "m-cell"

                                                    addIdMorphism =
                                                        if viewConfig.showIdentities && domId == codId then
                                                            (::) (idMorphism domObj.label)

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
    in
    Html.div []
        [ Html.h3 [] [ Html.text "Morphisms" ]
        , content
        ]


viewCompositionTable : Model -> Html Msg
viewCompositionTable { cat, viewConfig } =
    let
        morphs =
            Cat.listComposableMorphisms viewConfig.showIdentities cat

        getObjLabel objId =
            Maybe.withDefault "TODO???" <| Maybe.map .label <| Dict.get objId cat.objects

        morphHeaderCell mor =
            Html.th [] [ viewMorphism cat mor ]

        compositionTable =
            if List.isEmpty morphs.rows || List.isEmpty morphs.columns then
                Html.div []
                    [ Html.text <|
                        if viewConfig.showIdentities then
                            "There are no composable morphisms"

                        else
                            "There are no composable non-identity morphisms"
                    ]

            else
                let
                    homSets =
                        Cat.getHomSets cat
                in
                Html.table []
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
                                                if Cat.getCodId mor1 /= Cat.getDomId mor2 then
                                                    -- the 2 moprhisms are not composable
                                                    Html.td [] []

                                                else
                                                    case mor1 of
                                                        Identity objId1 ->
                                                            case mor2 of
                                                                Identity _ ->
                                                                    Html.td [] [ idMorphism (getObjLabel objId1) ]

                                                                NonIdentity morphId2 _ ->
                                                                    Html.td [] [ morphismWithId morphId2 ]

                                                        NonIdentity morphId1 nonIdM1 ->
                                                            case mor2 of
                                                                Identity _ ->
                                                                    Html.td [] [ morphismWithId morphId1 ]

                                                                NonIdentity morphId2 nonIdM2 ->
                                                                    let
                                                                        homSet =
                                                                            Maybe.withDefault Set.empty <| Dict.get ( nonIdM1.domId, nonIdM2.codId ) homSets

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
                                                                            composition =
                                                                                Dict.get ( morphId1, morphId2 ) cat.composition

                                                                            unfedOption =
                                                                                Html.option [ A.selected <| composition == Nothing ] [ Html.text "?" ]

                                                                            addIdentity =
                                                                                if isIdentityAvailable then
                                                                                    (::)
                                                                                        (Html.option
                                                                                            [ -- hack: since we don't store ID morphisms explicitly, we use negation of objId to represent them
                                                                                              A.selected <| composition == Just (negate nonIdM1.domId)
                                                                                            , A.value <| String.fromInt (negate nonIdM1.domId)
                                                                                            ]
                                                                                            [ idMorphism (getObjLabel nonIdM1.domId) ]
                                                                                        )

                                                                                else
                                                                                    identity

                                                                            morphOption morphId =
                                                                                Html.option
                                                                                    [ A.selected <| composition == Just morphId
                                                                                    , A.value <| String.fromInt morphId
                                                                                    ]
                                                                                    [ morphismWithId morphId ]
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
                                            )
                                            morphs.columns
                            )
                            morphs.rows
                    ]

        associativityDetails =
            let
                assocCheck =
                    Cat.checkAssociativity viewConfig.showIdentities cat
            in
            Html.details []
                [ Html.summary [] [ Html.text "Associativity" ]
                , Html.p [] [ Html.text "∀ a, b, c, d ∈ ob(C), f ∈ hom(a, b), g ∈ hom(b, c), h ∈ hom(c, d): (f;g);h = f;(g;h)" ]
                , viewIf (assocCheck.countAssocTriples > 0) <|
                    Html.p [ A.style "color" "green" ]
                        -- TODO pluralize
                        [ Html.text <| String.fromInt assocCheck.countAssocTriples ++ " triples satisfy the equation." ]
                , viewIf (assocCheck.countUndefinedTriples > 0) <|
                    Html.p [ A.style "color" "orange" ]
                        [ Html.text <| String.fromInt assocCheck.countUndefinedTriples ++ " triples could not be checked, because composition not defined for them." ]
                , viewIf (assocCheck.countNotAssociativeTriples > 0) <|
                    Html.p [ A.style "color" "red" ]
                        [ Html.text <| String.fromInt assocCheck.countNotAssociativeTriples ++ " triples violate the equation:"
                        , assocCheck.notAssociativeWitnesses
                            |> List.map (viewNotAssociativeWitness cat)
                            |> Html.ul []
                        ]
                ]
    in
    Html.div []
        [ Html.h3 [] [ Html.text "Composition" ]
        , compositionTable
        , associativityDetails
        ]


viewIf : Bool -> Html msg -> Html msg
viewIf cond content =
    if cond then
        content

    else
        Html.text ""


viewNotAssociativeWitness : Category -> NotAssociativeWitness -> Html msg
viewNotAssociativeWitness cat { m1, m2, m3, m1m2, m1m2_m3, m2m3, m1_m2m3 } =
    let
        vm =
            viewMorphism cat
    in
    Html.li []
        [ Html.text "("
        , vm m1
        , Html.text ";"
        , vm m2
        , Html.text ");"
        , vm m3
        , Html.text " = "
        , vm m1m2
        , Html.text ";"
        , vm m3
        , Html.text " = "
        , vm m1m2_m3
        , Html.text " ≠ "
        , vm m1_m2m3
        , Html.text " = "
        , vm m1
        , Html.text ";"
        , vm m2m3
        , Html.text " = "
        , vm m1
        , Html.text ";("
        , vm m2
        , Html.text ";"
        , vm m3
        , Html.text ")"
        ]


idMorphism : String -> Html msg
idMorphism domLbl =
    morphism "id" domLbl


morphismWithId : Int -> Html msg
morphismWithId morphId =
    morphism "f" (String.fromInt morphId)


viewMorphism : Category -> Morphism -> Html msg
viewMorphism cat m =
    case m of
        Identity objId ->
            idMorphism
                (Maybe.withDefault "TODO??" <|
                    Maybe.map .label <|
                        Dict.get objId cat.objects
                )

        NonIdentity morphId _ ->
            morphismWithId morphId


morphism : String -> String -> Html msg
morphism name sub =
    Html.span [] [ Html.text name, Html.sub [] [ Html.text sub ] ]


objHeaderCell : Bool -> ( Int, Cat.Obj ) -> Html msg
objHeaderCell colorObjects ( _, obj ) =
    let
        attrs =
            if colorObjects then
                [ A.style "background-color" (Color.toRGBString obj.color) ]

            else
                []
    in
    Html.th
        attrs
        [ Html.text obj.label ]
