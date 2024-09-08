module Category exposing
    ( Category
    , Morphism(..)
    , addMorphism
    , addObject
    , deleteMorphism
    , deleteObject
    , empty
    , getHomSets
    , listFirstCompositionMorphisms
    , listSecondCompositionMorphisms
    , renderDot
    , setObjectLabel
    )

import Dict exposing (Dict)
import Set exposing (Set)



-- INVARIANT:
-- morphisms, dom, cod all contains same keys -- TODO perhaps enforce this by storing all in a single Dict?


type alias Category =
    { -- | object ID -> label
      objects : Dict Int String
    , -- | morphism ID -> label. We only explicitly represents non-Identity morphisms.
      -- Identities are implicitly assumed present
      morphisms : Dict Int String
    , -- | morphism ID -> domain object ID
      dom : Dict Int Int
    , -- | morphism ID -> codomain object ID
      cod : Dict Int Int
    , -- | (morphism ID : A -> B, morphism ID : B -> C) -> morphism ID : A -> C
      composition : Dict ( Int, Int ) Int

    -- auto-increment ID generating sequences
    , objectIdGen : Int
    , morphismIdGen : Int
    }


type Morphism
    = Identity Int -- Object ID
    | NonIdentity NonIdM


type alias NonIdM =
    { mId : Int
    , domId : Int
    , codId : Int
    }


{-| List of morphisms for composition table.
By first I mean f in (f :: A -> B, g :: B -> C)

Ordered first by codomain, then by domain.

-}
listFirstCompositionMorphisms : Bool -> Category -> List Morphism
listFirstCompositionMorphisms includeIdentities cat =
    []


listSecondCompositionMorphisms : Bool -> Category -> List Morphism
listSecondCompositionMorphisms includeIdentities cat =
    []


{-| (domId, codId) -> Set morphism IDs
-}
getHomSets : Category -> Dict ( Int, Int ) (Set Int)
getHomSets cat =
    Dict.foldl
        (\morphId _ acc ->
            case Dict.get morphId cat.dom of
                Just domId ->
                    case Dict.get morphId cat.cod of
                        Just codId ->
                            Dict.update ( domId, codId )
                                (\maybeSet ->
                                    case maybeSet of
                                        Just set ->
                                            Just (Set.insert morphId set)

                                        Nothing ->
                                            Just (Set.singleton morphId)
                                )
                                acc

                        Nothing ->
                            acc

                Nothing ->
                    acc
        )
        Dict.empty
        cat.morphisms


empty : Category
empty =
    { objects = Dict.empty
    , morphisms = Dict.empty
    , dom = Dict.empty
    , cod = Dict.empty
    , composition = Dict.empty
    , objectIdGen = 0
    , morphismIdGen = 0
    }


addObject : Category -> Category
addObject cat =
    let
        objId =
            cat.objectIdGen

        lbl =
            -- TODO come up with better way to generate object labels, maybe do some wrap around..
            String.fromChar <| Char.fromCode <| 65 {- ASCII code of 'A' -} + objId
    in
    { cat
        | objects = Dict.insert objId lbl cat.objects
        , objectIdGen = objId + 1
    }


setObjectLabel : Int -> String -> Category -> Category
setObjectLabel objId label cat =
    { cat | objects = Dict.update objId (\_ -> Just label) cat.objects }


deleteObject : Int -> Category -> Category
deleteObject objId cat =
    { cat
        | objects = Dict.remove objId cat.objects
        , dom = Dict.filter (\_ domId -> domId /= objId) cat.dom
        , cod = Dict.filter (\_ codId -> codId /= objId) cat.cod
    }


addMorphism : Int -> Int -> Category -> Category
addMorphism domId codId cat =
    let
        morphId =
            cat.morphismIdGen
    in
    { cat
      -- TODO set morphism labels to sensible default?
        | morphisms = Dict.insert morphId "" cat.morphisms
        , dom = Dict.insert morphId domId cat.dom
        , cod = Dict.insert morphId codId cat.cod
        , morphismIdGen = morphId + 1
    }


deleteMorphism : Int -> Category -> Category
deleteMorphism morphId cat =
    { cat
        | morphisms = Dict.remove morphId cat.morphisms
        , dom = Dict.remove morphId cat.dom
        , cod = Dict.remove morphId cat.cod
    }


renderDot : Category -> String
renderDot cat =
    let
        nodeLines =
            Dict.toList cat.objects
                |> List.map
                    (\( objId, lbl ) ->
                        -- TODO will need some escaping of label string?
                        String.fromInt objId ++ "[label=" ++ lbl ++ "]"
                    )

        edgeLines =
            Dict.foldl
                (\morphId lbl acc ->
                    case Dict.get morphId cat.dom of
                        Just domId ->
                            case Dict.get morphId cat.cod of
                                Just codId ->
                                    let
                                        morphLbl =
                                            if String.isEmpty lbl then
                                                "<f<sub>" ++ String.fromInt morphId ++ "</sub>>"

                                            else
                                                lbl
                                    in
                                    -- TODO will need some escaping of label string?
                                    (String.fromInt domId
                                        ++ "->"
                                        ++ String.fromInt codId
                                        ++ "[label="
                                        ++ morphLbl
                                        ++ "]"
                                    )
                                        :: acc

                                Nothing ->
                                    acc

                        Nothing ->
                            acc
                )
                []
                cat.morphisms
    in
    String.join ";" <|
        "digraph G{graph[rankdir=BT;splines=true;overlap=false]"
            :: "node[shape=circle;width=0.3;fixedsize=true]"
            :: "edge[arrowsize=0.5]"
            :: (nodeLines ++ edgeLines)
            ++ [ "}" ]
