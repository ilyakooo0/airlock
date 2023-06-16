module Ur.Da exposing (Da, posixToDa)

import BigInt exposing (BigInt)
import Time exposing (Posix)


type alias Da =
    BigInt


unixEpochStart : BigInt
unixEpochStart =
    BigInt.fromIntString "170141184475152167957503069145530368000" |> Maybe.withDefault (BigInt.fromInt 0)


second : BigInt
second =
    BigInt.fromIntString "18446744073709551616" |> Maybe.withDefault (BigInt.fromInt 0)


posixToDa : Posix -> Da
posixToDa p =
    BigInt.add (BigInt.div (BigInt.mul (BigInt.fromInt (Time.posixToMillis p)) second) (BigInt.fromInt 1000)) unixEpochStart
