module MidiChannel exposing (MidiChannel, fromInt, toInt)


type MidiChannel
    = MidiChannel Int


fromInt : Int -> MidiChannel
fromInt =
    MidiChannel << clamp 1 16


toInt : MidiChannel -> Int
toInt (MidiChannel i) =
    i
