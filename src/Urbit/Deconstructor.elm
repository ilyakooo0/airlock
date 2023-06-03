module Urbit.Deconstructor exposing
    ( Deconstructor
    , alt
    , bytes
    , cell
    , const
    , cord
    , float32
    , float64
    , int
    , list
    , llec
    , map
    , oneOf
    , run
    , runBytes
    , sig
    , signedInt
    , tape
    , tar
    )

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as BD
import Bytes.Encode as BE
import Bytes.Extra
import Urbit exposing (..)


type Deconstructor a b
    = Deconstructor (Noun -> a -> Maybe b)


run : Deconstructor (a -> a) a -> Noun -> Maybe a
run (Deconstructor f) noun =
    f noun identity


runBytes : Deconstructor (a -> a) a -> Bytes -> Maybe a
runBytes (Deconstructor f) bs =
    cue bs
        |> Maybe.andThen
            (\noun -> f noun identity)


const : Deconstructor (a -> a) a -> a -> Deconstructor c c
const (Deconstructor f) value =
    Deconstructor
        (\noun c ->
            case f noun identity of
                Just a ->
                    if a == value then
                        Just c

                    else
                        Nothing

                Nothing ->
                    Nothing
        )


cord : Deconstructor (String -> a) a
cord =
    Deconstructor
        (\x f ->
            case x of
                Atom bs ->
                    BD.decode (BD.string (Bytes.width bs)) bs
                        |> Maybe.map f

                Cell _ ->
                    Nothing
        )


tape : Deconstructor (String -> a) a
tape =
    list cord |> fmap String.concat


int : Deconstructor (Int -> a) a
int =
    Deconstructor
        (\x f ->
            case x of
                Atom bs ->
                    (case Bytes.width bs of
                        1 ->
                            BD.decode BD.unsignedInt8 bs

                        2 ->
                            BD.decode (BD.unsignedInt16 LE) bs

                        3 ->
                            BD.decode (BD.unsignedInt32 LE) (BE.encode (BE.sequence [ BE.bytes bs, BE.unsignedInt8 0 ]))

                        4 ->
                            BD.decode (BD.unsignedInt32 LE) bs

                        _ ->
                            Nothing
                    )
                        |> Maybe.map f

                Cell _ ->
                    Nothing
        )


signedInt : Deconstructor (Int -> a) a
signedInt =
    int
        |> fmap
            (\i ->
                if Bitwise.and 1 i == 1 then
                    -(Bitwise.shiftRightBy 1 (1 + i))

                else
                    Bitwise.shiftRightBy 1 i
            )


float32 : Deconstructor (Float -> a) a
float32 =
    Deconstructor
        (\x f ->
            case x of
                Atom bs ->
                    BD.decode (BD.float32 LE) bs
                        |> Maybe.map f

                Cell _ ->
                    Nothing
        )


float64 : Deconstructor (Float -> a) a
float64 =
    Deconstructor
        (\x f ->
            case x of
                Atom bs ->
                    BD.decode (BD.float64 LE) bs
                        |> Maybe.map f

                Cell _ ->
                    Nothing
        )


bytes : Deconstructor (Bytes -> a) a
bytes =
    Deconstructor
        (\n f ->
            case n of
                Atom bs ->
                    f bs |> Just

                Cell _ ->
                    Nothing
        )


sig : Deconstructor a a
sig =
    Deconstructor
        (\n a ->
            case n of
                Atom b ->
                    if (Bytes.Extra.toByteValues b |> List.filter (\x -> x /= 0)) == [] then
                        Just a

                    else
                        Nothing

                Cell _ ->
                    Nothing
        )


list : Deconstructor (a -> a) a -> Deconstructor (List a -> b) b
list (Deconstructor f) =
    Deconstructor
        (\n g ->
            let
                go n_ =
                    case n_ of
                        Atom _ ->
                            Just []

                        Cell ( el, tail ) ->
                            Maybe.map2 (\a b -> a :: b) (f el identity) (go tail)
            in
            go n |> Maybe.map g
        )


alt : Deconstructor a b -> Deconstructor a b -> Deconstructor a b
alt (Deconstructor f) (Deconstructor g) =
    Deconstructor
        (\n a ->
            case f n a of
                Just x ->
                    Just x

                Nothing ->
                    g n a
        )


oneOf : List (Deconstructor a b) -> Deconstructor a b
oneOf l =
    case l of
        [] ->
            Deconstructor (\_ _ -> Nothing)

        x :: xs ->
            alt x (oneOf xs)


tar : Deconstructor a a
tar =
    Deconstructor (\_ a -> Just a)


llec : Deconstructor a b -> Deconstructor b c -> Deconstructor a c
llec (Deconstructor r) (Deconstructor l) =
    Deconstructor
        (\noun a ->
            case noun of
                Cell ( lhs, rhs ) ->
                    r rhs a |> Maybe.andThen (\b -> l lhs b)

                Atom _ ->
                    Nothing
        )


cell : Deconstructor a b -> Deconstructor b c -> Deconstructor a c
cell (Deconstructor l) (Deconstructor r) =
    Deconstructor
        (\noun a ->
            case noun of
                Cell ( lhs, rhs ) ->
                    l lhs a |> Maybe.andThen (\b -> r rhs b)

                Atom _ ->
                    Nothing
        )


map : a -> Deconstructor a b -> Deconstructor (b -> c) c
map a (Deconstructor f) =
    Deconstructor (\noun g -> f noun a |> Maybe.map g)


fmap : (a -> b) -> Deconstructor (a -> c) c -> Deconstructor (b -> c) c
fmap f (Deconstructor g) =
    Deconstructor (\noun h -> g noun (f >> h))
