port module Main exposing (main)


import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, fieldset, input, label, legend, option, pre, select, text)
import Html.Attributes exposing (checked, name, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onChange)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)
import Microbrute as Microbrute exposing (Command, Setting, SettingsGroup, getCommandData, optionName, optionValue, updateSetting)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type MidiPortType
    = Input
    | Output


type alias MidiPort =
    { name : String
    , id : PortID
    , type_ : MidiPortType
    }


type alias MidiMessage =
    { data : String
    , from : PortID
    }


type PortID
    = PortID String


portIdToString : PortID -> String
portIdToString (PortID s) =
    s


type MidiMessageIn
    = MidiMessagePort MidiPort
    | MidiMessageMessage MidiMessage


type alias MidiSend =
    { to : PortID
    , data : List Int -- bytearray, uint8array
    }


type alias MidiSubscribe =
    { to : PortID }


type MidiMessageOut
    = MidiMessageSend MidiSend
    | MidiMessageSubscribe MidiSubscribe


midiMessageOutEncoder : MidiMessageOut -> Value
midiMessageOutEncoder msg =
    case msg of
        MidiMessageSend midiSend ->
            midiSendEncoder midiSend

        MidiMessageSubscribe midiSubscribe ->
            midiSubscribeEncoder midiSubscribe


midiSubscribeEncoder : MidiSubscribe -> Value
midiSubscribeEncoder { to } =
    Encode.object [ ( "to", portEncoder to ) ]


midiSendEncoder : MidiSend -> Value
midiSendEncoder { to, data } =
    Encode.object
        [ ( "to", portEncoder to )
        , ( "data", Encode.list Encode.int data )
        ]


portEncoder : PortID -> Value
portEncoder (PortID midiPort) =
    Encode.string midiPort


port midiReceiver : (Value -> msg) -> Sub msg


port midiSender : Value -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    midiReceiver (\s -> decode s |> toMsg)


decode : Value -> Result Decode.Error MidiMessageIn
decode x =
    Decode.decodeValue midiMessageInDecoder <| Debug.log (Debug.toString x) x


toMsg : Result Decode.Error MidiMessageIn -> Msg
toMsg v =
    case v of
        Err err ->
            Error (Debug.toString err)

        Ok (MidiMessagePort x) ->
            Port x

        Ok (MidiMessageMessage x) ->
            Message x


midiMessageInDecoder : Decoder MidiMessageIn
midiMessageInDecoder =
    Decode.oneOf
        [ Decode.map MidiMessageMessage messageDecoder
        , Decode.map MidiMessagePort portDecoder
        ]


messageDecoder : Decoder MidiMessage
messageDecoder =
    Decode.succeed MidiMessage
        |> required "data" Decode.string
        |> required "from" idDecoder


portDecoder : Decoder MidiPort
portDecoder =
    Decode.succeed MidiPort
        |> required "name" Decode.string
        |> required "id" idDecoder
        |> required "type" typeDecoder


idDecoder : Decoder PortID
idDecoder =
    Decode.map PortID Decode.string


typeDecoder : Decoder MidiPortType
typeDecoder =
    Decode.string |> Decode.andThen toPort


toPort : String -> Decoder MidiPortType
toPort t =
    case t of
        "input" ->
            Decode.succeed Input

        "output" ->
            Decode.succeed Output

        _ ->
            Decode.fail "invalid port type"



-- MODEL


type alias Model =
    { ports : List MidiPort
    , selectedOutputPort : Maybe MidiPort
    , messages : List MidiMessage
    , errors : List String
    , counter : Int
    , settings : List Microbrute.SettingsGroup
    }


initialModel : Model
initialModel =
    Model [] Nothing [] [] 0 Microbrute.settings


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = Increment
    | Decrement
    | Error String
    | Port MidiPort
    | Message MidiMessage
    | SetOpt Command
    | SelectMidiPort MidiPort
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Increment ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        Decrement ->
            ( { model | counter = model.counter - 1 }, Cmd.none )

        Error x ->
            ( { model | errors = x :: model.errors }, Cmd.none )

        Port x ->
            ( { model | ports = x :: model.ports }, Cmd.none )

        Message x ->
            ( { model | messages = x :: model.messages }, Cmd.none )

        SetOpt setting ->
            ( { model | settings = updateSetting model.settings setting }
            , Maybe.withDefault Cmd.none
                (flip Maybe.map
                    model.selectedOutputPort
                    (midiSender << midiMessageOutEncoder << toMidiMessage setting)
                )
            )

        SelectMidiPort midiPort ->
            ( { model | selectedOutputPort = Just midiPort }, Cmd.none )


toMidiMessage : Command -> MidiPort -> MidiMessageOut
toMidiMessage setting { id } =
    MidiMessageSend
        { to = id
        , data = getCommandData setting
        }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model.counter) ]
        , button [ onClick Increment ] [ text "+" ]
        , midiPortSelector <| List.filter (\{ type_ } -> type_ == Output) model.ports
        , optionsGroupsHtml model.settings
        , div [ style "display" "flex", style "justify-content" "space-between" ] [sliderGroupsHtml model.settings]
        , div [] (List.map (\x -> pre [] [ text <| Debug.toString x ]) model.settings)
        , div [] [ pre [] [ text <| Debug.toString model.selectedOutputPort ] ]
        , pre [] (List.map (\x -> text x.name) model.ports)
        , pre [] (List.map (\x -> text x.data) model.messages)
        , pre [] (List.map (\x -> text x) model.errors)
        ]


midiPortSelector : List MidiPort -> Html Msg
midiPortSelector xs =
    let
        d : Dict String MidiPort
        d =
            Dict.fromList <| zip (List.map (\{ id } -> portIdToString id) xs) xs

        handleInput =
            Maybe.withDefault NoOp << Maybe.map SelectMidiPort << flip Dict.get d
    in
    div []
        [ select [ onInput handleInput ]
            (option [] [ text "Select a MIDI interface …" ] :: List.map (\x -> option [ value <| portIdToString x.id ] [ text x.name ]) xs)
        ]


zip : List a -> List b -> List ( a, b )
zip =
    List.map2 Tuple.pair


flip : (c -> b -> a) -> b -> c -> a
flip fn b a =
    fn a b

sliderGroupsHtml : List SettingsGroup -> Html Msg
sliderGroupsHtml  =
  div [] << List.map sliderGroupHtml

sliderGroupHtml : SettingsGroup -> Html Msg
sliderGroupHtml settingsGroup =
    fieldset []
        ( legend [] [ text settingsGroup.name ]
        ::  sliders settingsGroup.settings)

sliders = List.map slider

optionsGroupsHtml : List SettingsGroup -> Html Msg
optionsGroupsHtml  =
  div [] << List.map optionsGroupHtml

optionsGroupHtml : SettingsGroup -> Html Msg
optionsGroupHtml settingsGroup =
    fieldset []
        [ legend []
            [ text settingsGroup.name ]
        , optionsHtml settingsGroup.settings
        ]

optionsHtml : List Setting -> Html Msg
optionsHtml settings =
    div [] <| List.map radioButtons settings



-- radioButtons : Option OptionType -> Html Msg


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry fn ( a, b ) =
    fn a b


css : List ( String, String ) -> List (Html.Attribute Msg)
css =
    List.map <| uncurry style


indexOf : List a -> a -> Int
indexOf list value =
    let
        f xs i =
            case xs of
                [] ->
                    -1

                v :: rest ->
                    if v == value then
                        i

                    else
                        f rest i + 1
    in
    f list 0


isJust : Maybe a -> Bool
isJust x =
    case x of
        Nothing ->
            False

        Just _ ->
            True


isNothing : Maybe a -> Bool
isNothing =
    not << isJust



-- d tag x =
--     Debug.log (tag ++ " " ++ Debug.toString x) x


slider : Setting -> Html Msg
slider { selected, values } =
    let
        valueToNumber =
            Maybe.map (String.fromInt << (-) (List.length values - 1) << indexOf values)

        maybeToList x =
            case x of
                Nothing ->
                    []

                Just v ->
                    [ v ]

        numberToValue =
            -- Maybe.andThen lookup << String.toInt
            Maybe.andThen lookup << Maybe.map ((-) (List.length values - 1)) << String.toInt

        lookup x =
            List.head <| List.drop x values

        handleChange value =
            case value of
                Nothing ->
                    NoOp

                Just x ->
                    SetOpt x
    in
    div [ style "margin" "1em 0" ]
        [ label [ style "display" "inline-block" ]
            [ div
                [ style "display" "flex"
                , style "flex-direction" "row"
                , style "justify-content" "center"
                ]
                [ input
                    ([ type_ "range"
                     , style "appearance" "slider-vertical"
                     , style "width" "1em"
                     , style "height" "5em"
                     , Html.Attributes.min "0"
                     , Html.Attributes.max (String.fromInt <| List.length values - 1)
                     , Html.Attributes.disabled <| isNothing <| valueToNumber selected
                     , onChange <| handleChange << numberToValue
                     ]
                        ++ List.map value
                            (maybeToList <| valueToNumber selected)
                    )
                    []
                , div
                    (css
                        [ ( "display", "flex" )
                        , ( "flex-direction", "column" )
                        , ( "justify-content", "space-between" )
                        ]
                    )
                    (List.map valueLabel values)
                ]
            , text (getOptionName values)
            ]
        ]


getOptionName : List Command -> String
getOptionName x =
    Maybe.withDefault "" <| Maybe.map optionName <| List.head x


valueLabel : Command -> Html msg
valueLabel val =
    div [] [ text (optionValue val) ]


radioButtons : Setting -> Html Msg
radioButtons { selected, values } =
    let
        name =
            getOptionName values
    in
    fieldset [] <|
        legend [] [ text name ]
            :: List.concatMap (createRadioButton name selected) values



createRadioButton : String -> Maybe Command -> Command -> List (Html Msg)
createRadioButton fieldSetName selected settingValue =
    let
        isChecked =
            Maybe.map ((==) settingValue) selected |> Maybe.withDefault False
    in
    [ label []
        [ input
            [ type_ "radio"
            , name fieldSetName
            , value (optionValue settingValue)
            , checked isChecked
            , onInput (\_ -> SetOpt settingValue)
            ]
            []
        , text (optionValue settingValue)
        ]
    ]
