module Ur exposing
    ( Agent
    , Atom
    , Mark
    , Noun(..)
    , Path
    , cue
    , jam
    , logIn
    , mat
    , rub
    )

import BitParser as BP exposing (BitParser)
import BitWriter as BW exposing (BitWriter)
import Bitwise
import Bytes exposing (Bytes)
import Bytes.Extra as Bytes
import Dict exposing (Dict)
import Http
import List.Extra as List


{-| An Urbit agent (app) name like `journal` or 'groups'.
-}
type alias Agent =
    String


{-| An Urbit subscription path.
-}
type alias Path =
    List String


type alias Mark =
    String


logIn : String -> String -> Cmd (Result Http.Error ())
logIn root password =
    Http.riskyRequest
        { url = root ++ "/~/login"
        , method = "POST"
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        , body =
            Http.stringBody
                "application/x-www-form-urlencoded; charset=utf-8"
                ("password=" ++ password)
        , expect = Http.expectWhatever identity
        }


type Noun
    = Cell ( Noun, Noun )
    | Atom Atom


type alias Atom =
    Bytes


jam : Noun -> Bytes
jam n =
    BW.run (jamWriter n BW.empty)



-- Does not use references because it is complex to implement in Elm and would probably lead
-- to poor performance


jamWriter : Noun -> BitWriter -> BitWriter
jamWriter noun writer =
    case noun of
        Atom atom ->
            writer
                |> BW.bit 0
                |> mat atom

        Cell ( a, b ) ->
            writer
                |> BW.bit 1
                |> BW.bit 0
                |> jamWriter a
                |> jamWriter b


cue : Bytes -> Maybe Noun
cue =
    BP.run (cueParser Dict.empty) >> Maybe.map Tuple.second


cueParser : Dict Int Noun -> BitParser ( Dict Int Noun, Noun )
cueParser refs =
    BP.getOffset
        |> BP.andThen
            (\offset ->
                BP.bit
                    |> BP.andThen
                        (\isAtom ->
                            if isAtom == 0 then
                                rub |> BP.map (\a -> ( Dict.insert offset (Atom a) refs, Atom a ))

                            else
                                BP.bit
                                    |> BP.andThen
                                        (\isRef ->
                                            if isRef == 0 then
                                                cueParser refs
                                                    |> BP.andThen
                                                        (\( refs_, a ) ->
                                                            cueParser refs_
                                                                |> BP.andThen
                                                                    (\( refs__, b ) ->
                                                                        let
                                                                            c =
                                                                                Cell ( a, b )
                                                                        in
                                                                        ( Dict.insert offset c refs__, c )
                                                                            |> BP.succeed
                                                                    )
                                                        )

                                            else
                                                rub
                                                    |> BP.andThen
                                                        (\ref ->
                                                            case
                                                                Dict.get
                                                                    (Bytes.toByteValues ref
                                                                        |> List.foldr
                                                                            (\b acc -> Bitwise.shiftLeftBy 8 acc |> Bitwise.or b)
                                                                            0
                                                                    )
                                                                    refs
                                                            of
                                                                Just n ->
                                                                    BP.succeed ( refs, n )

                                                                _ ->
                                                                    BP.fail
                                                        )
                                        )
                        )
            )


isSig : Bytes -> Bool
isSig bytes =
    Bytes.toByteValues bytes |> List.all (\x -> x == 0)


mat : Bytes -> BitWriter -> BitWriter
mat bytes writer =
    if isSig bytes then
        writer |> BW.bit 1

    else
        let
            bits =
                bytesToBits bytes |> List.dropWhileRight (\x -> x == 0)

            lengthBits =
                bits |> List.length |> intToBits |> List.reverse |> List.drop 1 |> List.reverse
        in
        writer
            |> BW.bit 0
            |> BW.bits (List.repeat (List.length lengthBits) 0)
            |> BW.bit 1
            |> BW.bits lengthBits
            |> BW.bits bits


rub : BitParser Bytes
rub =
    BP.bit
        |> BP.andThen
            (\zeroBit ->
                if zeroBit == 1 then
                    BP.succeed Bytes.empty

                else
                    let
                        countZeros n =
                            BP.bit
                                |> BP.andThen
                                    (\b ->
                                        if b == 0 then
                                            countZeros (n + 1)

                                        else
                                            BP.succeed n
                                    )
                    in
                    countZeros 0
                        |> BP.andThen
                            (\lengthOfLength ->
                                BP.rawBits lengthOfLength
                                    |> BP.andThen
                                        (\preLengthRawBits ->
                                            let
                                                length =
                                                    BP.bitsToInt (preLengthRawBits ++ [ 1 ])
                                            in
                                            BP.bits length
                                        )
                            )
            )


bytesToBits : Bytes -> List Int
bytesToBits bytes =
    case
        BP.run (BP.rawBits (Bytes.width bytes * 8)) bytes
    of
        Nothing ->
            []

        Just bits ->
            bits


intToBits : Int -> List Int
intToBits n =
    if n <= 0 then
        []

    else
        Bitwise.and 1 n :: intToBits (Bitwise.shiftRightBy 1 n)
