module Microbrute exposing
    ( Command
    , Setting
    , SettingsGroup
    , getCommandData
    , optionName
    , optionValue
    , settings
    , updateSetting
    )

import BendRange exposing (BendRange)
import MidiChannel exposing (MidiChannel)


type alias Setting =
    { selected : Maybe Command
    , values : List Command
    }


type alias SettingsGroup =
    { name : String
    , settings : List Setting
    }


sysexCommandList : List SysexCommand -> List Command
sysexCommandList =
    List.map SysexCommand


midiCommandList : List MidiCommand -> List Command
midiCommandList =
    List.map MidiCommand


settings : List SettingsGroup
settings =
    let
        mv cons list =
            midiCommandList <| List.map cons list

        sv cons list =
            sysexCommandList <| List.map cons list

        is cons list =
            { values = sv cons list, selected = Nothing }

        im cons list =
            { values = mv cons list, selected = Nothing }
    in
    [ { name = "Keyboard parameters"
      , settings =
            [ is KeyboardNotePriorityValue [ Low, Last, High ]
            , is VelocityResponse [ AntiLogarithmic, Logarithmic, Linear ]
            , im LocalControl [ Off, On ]
            ]
      }
    , { name = "Sequencer control"
      , settings =
            [ is SequencerPlayMode [ Hold, NoteOn ]
            , is SequencerRetrig [ None, Legato, Reset ]
            , is NextSequence [ InstantContinuation, InstantReset, End ]
            , is StepMode [ Gate, Clock ]
            , is StepSize [ ⲒⳆ4, ⲒⳆ8, ⲒⳆ16, ⲒⳆ32 ]
            ]
      }
    , { name = "MIDI channel select"
      , settings =
            [ is MidiTransmitChannel (List.map MidiChannel.fromInt <| List.range 1 16)
            , is MidiReceiveChannel (Right All :: (List.map (Left << MidiChannel.fromInt) <| List.range 1 16))
            ]
      }
    , { name = "Module parameters"
      , settings =
            [ is LfoKeyRetriggerValue [ Off, On ]
            , is EnvelopeLegatoModeValue [ Off, On ]
            , is BendRange (List.map BendRange.fromInt <| List.range 1 12)
            , is GateLength [ Long, Medium, Short ]
            , is SyncSource [ Auto, Internal, External ]
            ]
      }
    ]


updateSetting : List SettingsGroup -> Command -> List SettingsGroup
updateSetting settingsGroups newValue =
    List.map (replace_ newValue) settingsGroups


replace_ : Command -> SettingsGroup -> SettingsGroup
replace_ newValue settingsGroup =
    { settingsGroup | settings = List.map (replace newValue) settingsGroup.settings }


replace : Command -> Setting -> Setting
replace newValue setting =
    if List.member newValue setting.values then
        { setting | selected = Just newValue }

    else
        setting



-- Stand-in type for an 8-bit byte


type alias Byte =
    Int


getCommandData : Command -> List Byte
getCommandData cmd =
    case cmd of
        MidiCommand x ->
            getMidiCommand x

        SysexCommand x ->
            getSysexCommand x


getMidiCommand : MidiCommand -> List Byte
getMidiCommand cmd =
    (case cmd of
        LocalControl _ ->
            [ 176 + 0 -- channel mode message + channel number from 0 to 15
            , 122 -- command
            ]
    )
        ++ [ case cmd of
                LocalControl Off ->
                    0

                LocalControl On ->
                    127
           ]


getSysexCommand : SysexCommand -> List Byte
getSysexCommand setting =
    getSysexBytes 0x00 <| getCommandBytes setting


getCommandBytes : SysexCommand -> List Byte
getCommandBytes setting =
    let
        settingByte =
            getSettingsByte setting

        valueByte =
            getValueByte setting
    in
    [ settingByte, valueByte ]



-- https://github.com/jmatraszek/microbrust/blob/master/src/interface/request_handler.rs
-- https://github.com/jmatraszek/microbrust/blob/master/src/interface/response_handler.rs
-- https://bulldogjob.com/news/97-how-i-reverse-engineered-synthesizer-protocol


channelModeMessageHeader : Byte -> Byte
channelModeMessageHeader channel =
    176 + channel


localOffMessage : List Byte
localOffMessage =
    [ 122, 0 ]


localOnMessage : List Byte
localOnMessage =
    [ 122, 127 ]


getSysexBytes : Byte -> List Byte -> List Byte
getSysexBytes counter commandBytes =
    let
        sysexStartByte =
            0xF0

        sysexEndByte =
            0xF7

        arturiaMidiID =
            [ 0x00
            , 0x20
            , 0x6B
            ]

        microBruteMidiID =
            0x05
    in
    sysexStartByte
        :: arturiaMidiID
        ++ [ microBruteMidiID, 0x01, counter ]
        ++ commandBytes
        ++ [ sysexEndByte ]



-- note: for the request, the commandValue is incremented by 1, so requesting note
-- priority becomes 0x0c


requestSavedSettingsCommandTypeByte : Byte
requestSavedSettingsCommandTypeByte =
    0x00


type Command
    = MidiCommand MidiCommand
    | SysexCommand SysexCommand


type OnOff
    = On
    | Off


type MidiCommand
    = LocalControl OnOff


type NotePriority
    = Low
    | Last
    | High


type VelocityResponse
    = Linear
    | Logarithmic
    | AntiLogarithmic


type PlayMode
    = NoteOn
    | Hold


type Retrig
    = Reset
    | Legato
    | None


type NextSequence
    = End
    | InstantReset
    | InstantContinuation


type StepMode
    = Clock
    | Gate


type StepSize
    = ⲒⳆ4
    | ⲒⳆ8
    | ⲒⳆ16
    | ⲒⳆ32


type GateLength
    = Short
    | Medium
    | Long


type SyncSource
    = External
    | Internal
    | Auto


type All
    = All


type Either a b
    = Left a
    | Right b


type
    SysexCommand
    -- Keyboard parameters
    = KeyboardNotePriorityValue NotePriority
    | VelocityResponse VelocityResponse
      -- Sequencer control
    | SequencerPlayMode PlayMode
    | SequencerRetrig Retrig
    | NextSequence NextSequence
    | StepMode StepMode
    | StepSize StepSize
      -- MIDI channel select
    | MidiTransmitChannel MidiChannel
    | MidiReceiveChannel (Either MidiChannel All)
      -- Module parameters
    | LfoKeyRetriggerValue OnOff
    | EnvelopeLegatoModeValue OnOff
    | BendRange BendRange
    | GateLength GateLength
    | SyncSource SyncSource


optionName : Command -> String
optionName cmd =
    case cmd of
        MidiCommand x ->
            midiCommandName x

        SysexCommand x ->
            sysexCommandName x


midiCommandName : MidiCommand -> String
midiCommandName option =
    case option of
        LocalControl _ ->
            "Local Control"


sysexCommandName : SysexCommand -> String
sysexCommandName option =
    case option of
        LfoKeyRetriggerValue _ ->
            "LFO Key Retrigger"

        EnvelopeLegatoModeValue _ ->
            "Envelope Legato Mode"

        KeyboardNotePriorityValue _ ->
            "Note Priority"

        VelocityResponse _ ->
            "Velocity Response"

        SequencerPlayMode _ ->
            "Play Mode"

        SequencerRetrig _ ->
            "Seq Retrig"

        NextSequence _ ->
            "Next Seq"

        StepMode _ ->
            "Step On"

        StepSize _ ->
            "Step"

        MidiTransmitChannel _ ->
            "Transmit Channel"

        MidiReceiveChannel _ ->
            "Receive Channel"

        BendRange _ ->
            "Bend Range"

        GateLength _ ->
            "Gate Length"

        SyncSource _ ->
            "Sync Source"


optionValue : Command -> String
optionValue cmd =
    case cmd of
        MidiCommand x ->
            midiCommandValue x

        SysexCommand x ->
            sysexCommandValue x


midiCommandValue : MidiCommand -> String
midiCommandValue option =
    case option of
        LocalControl Off ->
            "Off"

        LocalControl On ->
            "On"


sysexCommandValue : SysexCommand -> String
sysexCommandValue option =
    case option of
        LfoKeyRetriggerValue Off ->
            "Off"

        LfoKeyRetriggerValue On ->
            "On"

        EnvelopeLegatoModeValue On ->
            "On"

        EnvelopeLegatoModeValue Off ->
            "Off"

        KeyboardNotePriorityValue High ->
            "High"

        KeyboardNotePriorityValue Last ->
            "Last"

        KeyboardNotePriorityValue Low ->
            "Low"

        VelocityResponse Linear ->
            "Linear"

        VelocityResponse Logarithmic ->
            "Log"

        VelocityResponse AntiLogarithmic ->
            "Antilog"

        SequencerPlayMode NoteOn ->
            "Note On"

        SequencerPlayMode Hold ->
            "Hold"

        SequencerRetrig Reset ->
            "Reset"

        SequencerRetrig Legato ->
            "Legato"

        SequencerRetrig None ->
            "None"

        NextSequence End ->
            "End"

        NextSequence InstantReset ->
            "Instant reset"

        NextSequence InstantContinuation ->
            "Instant continuation"

        StepMode Clock ->
            "Clock"

        StepMode Gate ->
            "Gate"

        StepSize ⲒⳆ4 ->
            "1⁄4"

        StepSize ⲒⳆ8 ->
            "1⁄8"

        StepSize ⲒⳆ16 ->
            "1⁄16"

        StepSize ⲒⳆ32 ->
            "1⁄32"

        MidiTransmitChannel value ->
            String.fromInt <| MidiChannel.toInt value

        MidiReceiveChannel (Left value) ->
            String.fromInt <| MidiChannel.toInt value

        MidiReceiveChannel (Right All) ->
            "All"

        BendRange value ->
            String.fromInt <| BendRange.toInt value

        GateLength Short ->
            "Short"

        GateLength Medium ->
            "Medium"

        GateLength Long ->
            "Long"

        SyncSource External ->
            "External"

        SyncSource Internal ->
            "Internal"

        SyncSource Auto ->
            "Auto"


getSettingsByte : SysexCommand -> Int
getSettingsByte setting =
    case setting of
        LfoKeyRetriggerValue _ ->
            0x0F

        EnvelopeLegatoModeValue _ ->
            0x0D

        KeyboardNotePriorityValue _ ->
            0x0B

        VelocityResponse _ ->
            0x11

        SequencerPlayMode _ ->
            0x2E

        SequencerRetrig _ ->
            0x34

        NextSequence _ ->
            0x32

        StepMode _ ->
            0x2A

        StepSize _ ->
            0x38

        MidiTransmitChannel _ ->
            0x07

        MidiReceiveChannel _ ->
            0x05

        BendRange _ ->
            0x2C

        GateLength _ ->
            0x36

        SyncSource _ ->
            0x3C


getValueByte : SysexCommand -> Int
getValueByte setting =
    case setting of
        LfoKeyRetriggerValue value ->
            case value of
                Off ->
                    0x00

                On ->
                    0x01

        KeyboardNotePriorityValue value ->
            case value of
                Last ->
                    0x00

                Low ->
                    0x01

                High ->
                    0x02

        EnvelopeLegatoModeValue value ->
            case value of
                Off ->
                    0x00

                On ->
                    0x01

        VelocityResponse value ->
            case value of
                Linear ->
                    0x00

                Logarithmic ->
                    0x01

                -- swapped?
                AntiLogarithmic ->
                    0x02

        SequencerPlayMode value ->
            case value of
                Hold ->
                    0x00

                NoteOn ->
                    0x01

        SequencerRetrig value ->
            case value of
                Reset ->
                    0x00

                Legato ->
                    0x01

                None ->
                    0x02

        NextSequence value ->
            case value of
                End ->
                    0x00

                InstantReset ->
                    0x01

                InstantContinuation ->
                    0x02

        StepMode value ->
            case value of
                Clock ->
                    0x00

                Gate ->
                    0x01

        StepSize value ->
            case value of
                ⲒⳆ4 ->
                    0x04

                ⲒⳆ8 ->
                    0x08

                ⲒⳆ16 ->
                    0x10

                ⲒⳆ32 ->
                    0x20

        MidiTransmitChannel value ->
            MidiChannel.toInt value - 1

        MidiReceiveChannel (Left value) ->
            MidiChannel.toInt value - 1

        MidiReceiveChannel (Right All) ->
            0x10

        BendRange value ->
            BendRange.toInt value

        GateLength value ->
            case value of
                Short ->
                    0x01

                Medium ->
                    0x02

                Long ->
                    0x03

        SyncSource value ->
            case value of
                Auto ->
                    0x00

                Internal ->
                    0x01

                External ->
                    0x02
