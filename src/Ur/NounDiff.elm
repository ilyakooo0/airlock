module Ur.NounDiff exposing (Patch, apply, deconstructPatch)

import Bytes.Extra
import Dict exposing (Dict)
import Ur.Deconstructor as D
import Ur.Types exposing (..)


type Patch
    = PatchCell Patch Patch
    | Diff DelDiff InsDiff


deconstructPatch : () -> D.Deconstructor Patch
deconstructPatch () =
    D.oneOf
        [ D.cell (D.const D.cord "cell")
            (D.cell
                (D.lazy deconstructPatch)
                (D.lazy deconstructPatch)
            )
            |> D.map (\( (), ( a, b ) ) -> PatchCell a b)
        , D.cell (D.const D.cord "diff") (D.cell (D.lazy deconstructDel) (D.lazy deconstructIns))
            |> D.map (\( (), ( a, b ) ) -> Diff a b)
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


deconstructDel : () -> D.Deconstructor DelDiff
deconstructDel () =
    D.oneOf
        [ D.cell (D.const D.cord "ignore") D.ignore |> D.map (\( (), () ) -> Ignore)
        , D.cell (D.const D.cord "hole") D.int |> D.map (\( (), x ) -> DelHole x)
        , D.cell (D.const D.cord "cell")
            (D.cell
                (D.lazy deconstructDel)
                (D.lazy deconstructDel)
            )
            |> D.map (\( (), ( lhs, rhs ) ) -> DelCell lhs rhs)
        ]


type InsDiff
    = InsAtom Atom
    | InsHole Int
    | InsCell InsDiff InsDiff


deconstructIns : () -> D.Deconstructor InsDiff
deconstructIns () =
    D.oneOf
        [ D.cell (D.const D.cord "hole") D.int |> D.map (\( (), x ) -> InsHole x)
        , D.cell (D.const D.cord "atom") D.bytes |> D.map (\( (), bs ) -> InsAtom bs)
        , D.cell (D.const D.cord "cell")
            (D.cell
                (D.lazy deconstructIns)
                (D.lazy deconstructIns)
            )
            |> D.map (\( (), ( a, b ) ) -> InsCell a b)
        ]
