module Microbrute exposing (Command, Setting, SettingsGroup, getCommandData, optionName, optionValue, settings, updateSetting)


type alias Setting =
    { selected : Maybe Command
    , values : List Command
    }

type alias SettingsGroup = {
    name: String,
    settings: List Setting
  }

sysexCommandList = List.map SysexCommand
midiCommandList = List.map MidiCommand

keyboardParameters : SettingsGroup
keyboardParameters =
    { name = "Keyboard parameters"
    , settings = [ 
        { values = sysexCommandList <| List.map KeyboardNotePriorityValue [ High, Last, Low ]
        , selected =  Nothing
        },
        -- velocity response,
        { values = midiCommandList <| List.map LocalControl [ On, Off ]
        , selected =  Nothing
        }
    ]}

moduleParameters : SettingsGroup
moduleParameters =
    { name = "Module parameters"
    , settings = [ 
        { values = sysexCommandList <| List.map LfoKeyRetriggerValue [ On, Off ]
        , selected =  Nothing
        },
        { values = sysexCommandList <| List.map EnvelopeLegatoModeValue [ On, Off ]
        , selected =  Nothing
        }
        -- bend range 1-12
        -- gate
        -- sync
    ]}


settings : List SettingsGroup
settings =
  [keyboardParameters
  ,moduleParameters
  ]


updateSetting : List SettingsGroup -> Command -> List SettingsGroup
updateSetting settingsGroups newValue =
    List.map (replace_ newValue) settingsGroups


replace_ : Command -> SettingsGroup -> SettingsGroup
replace_ newValue settingsGroup =
  { settingsGroup | settings = List.map (replace newValue) settingsGroup.settings}

replace : Command -> Setting -> Setting
replace newValue setting =
    if List.member newValue setting.values then
        { setting | selected = Just newValue }

    else
        setting


getCommandData : Command -> List Int
getCommandData cmd =
    case cmd of
        MidiCommand x ->
            getMidiCommand x

        SysexCommand x ->
            getSysexCommand x


getMidiCommand : MidiCommand -> List Int
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


getSysexCommand : SysexCommand -> List Int
getSysexCommand setting =
    getSysexBytes 0x00 <| getCommandBytes setting


getCommandBytes : SysexCommand -> List Int
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


channelModeMessageHeader channel =
    176 + channel


localOffMessage =
    [ 122, 0 ]


localOnMessage =
    [ 122, 127 ]


getSysexBytes : Int -> List Int -> List Int
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


requestSavedSettingsCommandTypeByte : Int
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


type SysexCommand
    = LfoKeyRetriggerValue OnOff
    | EnvelopeLegatoModeValue OnOff
    | KeyboardNotePriorityValue NotePriority


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


getSettingsByte : SysexCommand -> Int
getSettingsByte setting =
    case setting of
        LfoKeyRetriggerValue _ ->
            0x0F

        EnvelopeLegatoModeValue _ ->
            0x0D

        KeyboardNotePriorityValue _ ->
            0x0B


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
