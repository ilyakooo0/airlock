module Ur.NounDiff exposing (Patch, apply, deconstructPatch)

import Bytes.Extra
import Dict exposing (Dict)
import Ur.Deconstructor as D
import Ur.Types exposing (..)


type Patch
    = PatchCell Patch Patch
    | Diff DelDiff InsDiff


deconstructPatch : D.Deconstructor (Patch -> c) c
deconstructPatch =
    D.oneOf
        [ D.cell (D.const D.cord "cell")
            (D.cell
                (D.lazy (\() -> deconstructPatch_))
                (D.lazy (\() -> deconstructPatch_))
            )
            |> D.map PatchCell
        , D.cell (D.const D.cord "diff") (D.cell deconstructDel deconstructIns) |> D.map Diff
        ]


deconstructPatch_ : D.Deconstructor (Patch -> c) c
deconstructPatch_ =
    D.oneOf
        [ D.cell (D.const D.cord "cell")
            (D.cell
                deconstructPatch
                deconstructPatch
            )
            |> D.map PatchCell
        , D.cell (D.const D.cord "diff") (D.cell deconstructDel deconstructIns) |> D.map Diff
        ]


apply : Patch -> Noun -> Noun
apply patch noun =
    case ( patch, noun ) of
        ( PatchCell lhs rhs, Cell ( lhs_, rhs_ ) ) ->
            Cell ( apply lhs lhs_, apply rhs rhs_ )

        ( Diff delDiff insDiff, _ ) ->
            ins insDiff (del delDiff noun)

        _ ->
            noun


del : DelDiff -> Noun -> Dict Int Noun
del diff noun =
    let
        go : DelDiff -> Noun -> Dict Int Noun -> Dict Int Noun
        go diff_ noun_ dict =
            case ( diff_, noun_ ) of
                ( Ignore, _ ) ->
                    dict

                ( DelCell lhsDiff rhsDiff, Cell ( lhs, rhs ) ) ->
                    go lhsDiff lhs (go rhsDiff rhs dict)

                ( DelHole hole, _ ) ->
                    -- There should be a continuity check here.
                    -- Check that the noun_ is equal to whatever the hole maps to in dict.
                    -- I don't include it for speed and laziness reasons.
                    Dict.insert hole noun_ dict

                _ ->
                    dict
    in
    go diff noun Dict.empty


ins : InsDiff -> Dict Int Noun -> Noun
ins diff dict =
    case diff of
        InsAtom a ->
            Atom a

        InsHole hole ->
            Dict.get hole dict |> Maybe.withDefault (Atom Bytes.Extra.empty)

        InsCell lhs rhs ->
            Cell ( ins lhs dict, ins rhs dict )


type DelDiff
    = Ignore
    | DelHole Int
    | DelCell DelDiff DelDiff


deconstructDel : D.Deconstructor (DelDiff -> c) c
deconstructDel =
    D.oneOf
        [ D.cell (D.const D.cord "ignore") D.ignore |> D.map Ignore
        , D.cell (D.const D.cord "hole") D.int |> D.map DelHole
        , D.cell (D.const D.cord "cell")
            (D.cell
                deconstructDel_
                deconstructDel_
            )
            |> D.map DelCell
        ]


deconstructDel_ : D.Deconstructor (DelDiff -> c) c
deconstructDel_ =
    D.oneOf
        [ D.const D.cord "ignore" |> D.map Ignore
        , D.cell (D.const D.cord "hole") D.int |> D.map DelHole
        , D.cell (D.const D.cord "cell")
            (D.cell
                (D.lazy (\() -> deconstructDel))
                (D.lazy (\() -> deconstructDel))
            )
            |> D.map DelCell
        ]


type InsDiff
    = InsAtom Atom
    | InsHole Int
    | InsCell InsDiff InsDiff


deconstructIns : D.Deconstructor (InsDiff -> c) c
deconstructIns =
    D.oneOf
        [ D.cell (D.const D.cord "hole") D.int |> D.map InsHole
        , D.cell (D.const D.cord "atom") D.bytes |> D.map InsAtom
        , D.cell (D.const D.cord "cell")
            (D.cell
                deconstructIns_
                deconstructIns_
            )
            |> D.map InsCell
        ]


deconstructIns_ : D.Deconstructor (InsDiff -> c) c
deconstructIns_ =
    D.oneOf
        [ D.cell (D.const D.cord "hole") D.int |> D.map InsHole
        , D.cell (D.const D.cord "atom") D.bytes |> D.map InsAtom
        , D.cell (D.const D.cord "cell")
            (D.cell
                (D.lazy (\() -> deconstructIns))
                (D.lazy (\() -> deconstructIns))
            )
            |> D.map InsCell
        ]
