module BendRange exposing (BendRange, fromInt, toInt)


type BendRange
    = BendRange Int


fromInt : Int -> BendRange
fromInt =
    BendRange << clamp 1 12


toInt : BendRange -> Int
toInt (BendRange i) =
    i
