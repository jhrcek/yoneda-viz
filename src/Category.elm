module Category exposing
    ( Category
    , CompositionCheck
    , Morphism(..)
    , NotAssociativeWitness
    , addMorphism
    , addObject
    , checkAssociativity
    , defineComposition
    , deleteMorphism
    , deleteObject
    , empty
    , getCodId
    , getDomId
    , getHomSets
    , listComposableMorphisms
    , renderDot
    , setObjectLabel
    , undefineComposition
    )

import Basics.Extra exposing (uncurry)
import Dict exposing (Dict)
import Set exposing (Set)


type alias Category =
    { -- | object ID -> label
      objects : Dict Int String
    , -- | morphism ID -> NonIdM. We only explicitly represents non-Identity morphisms.
      -- Identities are implicitly assumed present
      morphisms : Dict Int NonIdM
    , -- | (morphism ID : A -> B, morphism ID : B -> C) -> morphism ID : A -> C
      --  Since 2 morphisms A -> X, X -> A can compose to identity on A, we represent such compositions as negated objectID
      -- TODO come up with a better way to represent identity morphisms here ^
      composition : Dict ( Int, Int ) Int
    , -- | auto-increment ID generating sequences
      objectIdGen : Int
    , morphismIdGen : Int
    }


type Morphism
    = Identity Int {- <- obj ID -}
    | NonIdentity Int {- <- morp ID -} NonIdM


getDomId : Morphism -> Int
getDomId morph =
    case morph of
        Identity objId ->
            objId

        NonIdentity _ { domId } ->
            domId


getCodId : Morphism -> Int
getCodId morph =
    case morph of
        Identity objId ->
            objId

        NonIdentity _ { codId } ->
            codId


type alias NonIdM =
    { domId : Int
    , codId : Int
    , label : String
    }


{-| List morphisms for composition table.
We return 2 lists:

  - rows (containing Xs from X : A -> B ; Y : B -> C)
  - columns (containing Ys from X : A -> B ; Y : B -> C)

The rows are sorted by codomain ID, then by domain ID
The columns are sorted by domain ID, then by codomain ID so we can get composition table "clustered"
around "the object trhough which the morphisms are being composed"

-}
listComposableMorphisms : Bool -> Category -> { rows : List Morphism, columns : List Morphism }
listComposableMorphisms includeIdentities cat =
    let
        ms =
            List.map (uncurry NonIdentity) (Dict.toList cat.morphisms)
                ++ (if includeIdentities then
                        List.map Identity (Dict.keys cat.objects)

                    else
                        []
                   )

        rows =
            ms
                -- Only keep morphisms that can be composed with at least 1 morphism
                |> List.filter (\first -> List.any (\second -> getCodId first == getDomId second) ms)
                |> List.sortBy (\m -> ( getCodId m, getDomId m ))

        columns =
            ms
                -- Only keep morphisms that can be composed with at least 1 morphism
                |> List.filter (\second -> List.any (\first -> getCodId first == getDomId second) ms)
                |> List.sortBy (\m -> ( getDomId m, getCodId m ))
    in
    { rows = rows
    , columns = columns
    }


{-| (domId, codId) -> Set morphism IDs
-}
getHomSets : Category -> Dict ( Int, Int ) (Set Int)
getHomSets cat =
    Dict.foldl
        (\morphId { domId, codId } ->
            Dict.update ( domId, codId )
                (\maybeSet ->
                    case maybeSet of
                        Just set ->
                            Just (Set.insert morphId set)

                        Nothing ->
                            Just (Set.singleton morphId)
                )
        )
        Dict.empty
        cat.morphisms


empty : Category
empty =
    { objects = Dict.empty
    , morphisms = Dict.empty
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
    let
        ( msToDelete, msToKeep ) =
            Dict.partition (\_ m -> m.domId == objId || m.codId == objId) cat.morphisms

        morphIdsToDelete =
            Set.fromList (Dict.keys msToDelete)
    in
    { cat
        | objects = Dict.remove objId cat.objects
        , morphisms = msToKeep
        , composition =
            Dict.filter
                (\( firstId, secondId ) compId ->
                    not
                        (Set.member firstId morphIdsToDelete
                            || Set.member secondId morphIdsToDelete
                            || Set.member compId morphIdsToDelete
                        )
                )
                cat.composition
    }


addMorphism : Int -> Int -> Category -> Category
addMorphism domId codId cat =
    let
        morphId =
            cat.morphismIdGen
    in
    { cat
      -- TODO set morphism labels to sensible default?
        | morphisms = Dict.insert morphId { domId = domId, codId = codId, label = "" } cat.morphisms
        , morphismIdGen = morphId + 1
    }


deleteMorphism : Int -> Category -> Category
deleteMorphism morphId cat =
    { cat
        | morphisms = Dict.remove morphId cat.morphisms
        , composition =
            Dict.filter
                (\( firstId, secondId ) compId ->
                    not (morphId == firstId || morphId == secondId || morphId == compId)
                )
                cat.composition
    }


defineComposition : Int -> Int -> Int -> Category -> Category
defineComposition morphId1 morphId2 morphId cat =
    { cat | composition = Dict.insert ( morphId1, morphId2 ) morphId cat.composition }


undefineComposition : Int -> Int -> Category -> Category
undefineComposition morphId1 morphId2 cat =
    { cat | composition = Dict.remove ( morphId1, morphId2 ) cat.composition }


{-| NotAssociativeWitness
A triple of morphisms (m1, m2, m3) for which (m1;m2);m3 /= m1;(m2;m3)
-}
type alias NotAssociativeWitness =
    { m1 : Morphism
    , m2 : Morphism
    , m3 : Morphism
    , m1m2 : Morphism
    , m1m2_m3 : Morphism
    , m2m3 : Morphism
    , m1_m2m3 : Morphism
    }


type alias CompositionCheck =
    { -- Count of triples of morphisms (m1, m2, m3) that satisfy associativity, i.e. (m1;m2);m3 = m1;(m2;m3)
      countAssocTriples : Int
    , -- Count of triples  for which associativity couldn't be checked, because composition rule is not complete
      countUndefinedTriples : Int
    , -- Count triples that satisfy associativity
      countNotAssociativeTriples : Int
    , notAssociativeWitnesses : List NotAssociativeWitness
    }


emptyCompositionCheck : CompositionCheck
emptyCompositionCheck =
    CompositionCheck 0 0 0 []


incrementAssociative : CompositionCheck -> CompositionCheck
incrementAssociative check =
    { check | countAssocTriples = check.countAssocTriples + 1 }


incrementUndefined : CompositionCheck -> CompositionCheck
incrementUndefined check =
    { check | countUndefinedTriples = check.countUndefinedTriples + 1 }


addNonAssocWitness : NotAssociativeWitness -> CompositionCheck -> CompositionCheck
addNonAssocWitness witness check =
    { check
        | notAssociativeWitnesses = witness :: check.notAssociativeWitnesses
        , countNotAssociativeTriples = check.countNotAssociativeTriples + 1
    }


checkAssociativity : Bool -> Category -> CompositionCheck
checkAssociativity includeIdentities cat =
    let
        ms =
            List.map (uncurry NonIdentity) (Dict.toList cat.morphisms)
                ++ (if includeIdentities then
                        List.map Identity (Dict.keys cat.objects)

                    else
                        []
                   )

        -- This function assumes that m1 and m2 are composable
        lookupComposition m1 m2 =
            case ( m1, m2 ) of
                ( Identity _, other ) ->
                    Just other

                ( other, Identity _ ) ->
                    Just other

                ( NonIdentity morpId1 _, NonIdentity morpId2 _ ) ->
                    Dict.get ( morpId1, morpId2 ) cat.composition
                        |> Maybe.andThen
                            (\compId ->
                                if compId < 0 then
                                    Just (Identity (negate compId))

                                else
                                    Dict.get compId cat.morphisms
                                        |> Maybe.map (NonIdentity compId)
                            )
    in
    triples ms
        |> List.foldr
            (\( m1, m2, m3 ) ->
                -- Both m1;m2 and m2;m3 are composable
                if getCodId m1 == getDomId m2 && getCodId m2 == getDomId m3 then
                    let
                        leftAssoc =
                            lookupComposition m1 m2
                                |> Maybe.andThen (\m1m2 -> lookupComposition m1m2 m3 |> Maybe.map (Tuple.pair m1m2))

                        rightAssoc =
                            lookupComposition m2 m3
                                |> Maybe.andThen (\m2m3 -> lookupComposition m1 m2m3 |> Maybe.map (Tuple.pair m2m3))
                    in
                    Maybe.map2
                        (\( m1m2, m1m2_m3 ) ( m2m3, m1_m2m3 ) ->
                            if m1m2_m3 == m1_m2m3 then
                                incrementAssociative

                            else
                                addNonAssocWitness
                                    { m1 = m1
                                    , m2 = m2
                                    , m3 = m3
                                    , m1m2 = m1m2
                                    , m1m2_m3 = m1m2_m3
                                    , m2m3 = m2m3
                                    , m1_m2m3 = m1_m2m3
                                    }
                        )
                        leftAssoc
                        rightAssoc
                        |> Maybe.withDefault incrementUndefined

                else
                    identity
            )
            emptyCompositionCheck


triples : List a -> List ( a, a, a )
triples xs =
    List.concatMap
        (\x ->
            List.concatMap
                (\y ->
                    List.map
                        (\z -> ( x, y, z ))
                        xs
                )
                xs
        )
        xs


renderDot : Bool -> Category -> String
renderDot showIdentities cat =
    let
        objList =
            Dict.toList cat.objects

        nodeLines =
            List.map
                (\( objId, lbl ) ->
                    -- TODO will need some escaping of label string?
                    String.fromInt objId ++ "[label=" ++ lbl ++ "]"
                )
                objList

        edgeLines =
            Dict.foldl
                (\morphId { label, domId, codId } ->
                    let
                        morphLbl =
                            if String.isEmpty label then
                                "<f<sub>" ++ String.fromInt morphId ++ "</sub>>"

                            else
                                label
                    in
                    (::)
                        (String.fromInt domId
                            ++ "->"
                            ++ String.fromInt codId
                            ++ "[label="
                            -- TODO will need some escaping of label string?
                            ++ morphLbl
                            ++ "]"
                        )
                )
                []
                cat.morphisms
                ++ (if showIdentities then
                        List.map
                            (\( objId, objLbl ) ->
                                String.fromInt objId
                                    ++ "->"
                                    ++ String.fromInt objId
                                    ++ "[label=<id<sub>"
                                    -- TODO will need some escaping of label string?
                                    ++ objLbl
                                    ++ "</sub>>]"
                            )
                            objList

                    else
                        []
                   )
    in
    String.join ";" <|
        "digraph G{graph[rankdir=BT;splines=true;overlap=false]"
            :: "node[shape=circle;width=0.3;fixedsize=true]"
            :: "edge[arrowsize=0.5]"
            :: (nodeLines ++ edgeLines)
            ++ [ "}" ]
