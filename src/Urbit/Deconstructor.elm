module Urbit.Deconstructor exposing
    ( Deconstructor
    , alt
    , cell
    , float32
    , float64
    , int
    , list
    , llec
    , map
    , run
    , runBytes
    , sig
    , string
    , tar
    )

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as BD
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


string : Deconstructor (String -> a) a
string =
    Deconstructor
        (\x f ->
            case x of
                Atom bs ->
                    BD.decode (BD.string (Bytes.width bs)) bs
                        |> Maybe.map f

                Cell _ ->
                    Nothing
        )


int : Deconstructor (Int -> a) a
int =
    Deconstructor
        (\x f ->
            case x of
                Atom bs ->
                    Bytes.Extra.toByteValues bs
                        |> List.foldr
                            (\b acc -> Bitwise.shiftLeftBy 8 acc |> Bitwise.or b)
                            0
                        |> f
                        |> Just

                Cell _ ->
                    Nothing
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
