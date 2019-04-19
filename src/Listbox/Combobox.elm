module Listbox.Combobox exposing
    ( Combobox, init, view, Instance, Label, label, labelledBy, noLabel
    , update, Msg, subscriptions
    , UpdateConfig, updateConfig, Behaviour
    , ViewConfig, viewConfig, Role(..), Visibility(..), Views
    )

{-|

@docs Combobox, init, view, Instance, Label, label, labelledBy, noLabel

@docs update, Msg, subscriptions


# Configuration


## Update

@docs UpdateConfig, updateConfig, Behaviour


## View

@docs ViewConfig, viewConfig, Role, Visibility, Views

-}

{-

   Copyright 2019 Fabian Kirchner

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

-}

import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Internal.KeyInfo as KeyInfo
import Json.Decode as Decode
import Listbox exposing (Entry, Listbox)
import Listbox.Unique


{-| TODO
-}
type Combobox a
    = Combobox (ComboboxData a)


type alias ComboboxData a =
    { focused : Bool
    , showRequested : Bool
    , closeRequested : Bool
    , preventBlur : Bool
    , listbox : Listbox
    , selection : Maybe a
    }


{-| TODO
-}
init : Combobox a
init =
    Combobox
        { focused = False
        , showRequested = False
        , closeRequested = False
        , preventBlur = False
        , listbox = Listbox.init
        , selection = Nothing
        }



---- VIEW


{-| TODO
-}
type alias Instance a msg =
    { id : String
    , label : Label
    , lift : Msg a -> msg
    }


{-| TODO
-}
type Label
    = LabelledBy String
    | Label String
    | NoLabel


{-| TODO
-}
labelledBy : String -> Label
labelledBy =
    LabelledBy


{-| TODO
-}
label : String -> Label
label =
    Label


{-| TODO
-}
noLabel : Label
noLabel =
    NoLabel


{-| TODO
-}
view :
    ViewConfig a divider
    -> Instance a msg
    -> List (Entry a divider)
    -> Combobox a
    -> String
    -> Html msg
view (ViewConfig config) instance entries (Combobox data) value =
    let
        addAriaControls attrs =
            if isOpen then
                Attributes.attribute "aria-controls" (listboxId instance) :: attrs

            else
                attrs

        addAriaLabelledBy attrs =
            case instance.label of
                LabelledBy id ->
                    Attributes.attribute "aria-labelledby" id :: attrs

                Label id ->
                    Attributes.attribute "aria-label" id :: attrs

                NoLabel ->
                    attrs

        addAriaActivedescendant attrs =
            case
                Listbox.focusedEntryId listboxViewConfig
                    listboxInstance
                    entries
                    data.listbox
            of
                Nothing ->
                    attrs

                Just id ->
                    Attributes.attribute "aria-activedescendant" id :: attrs

        listboxViewConfig =
            Listbox.viewConfig config.uniqueId
                { ul =
                    if isOpen then
                        Attributes.style "position" "absolute" :: config.views.ul

                    else
                        Attributes.style "display" "none"
                            :: Attributes.style "position" "absolute"
                            :: config.views.ul
                , liOption = config.views.liOption
                , liDivider = config.views.liDivider
                , empty = Html.text ""
                , focusable = False
                , markActiveDescendant = False
                }

        listboxInstance =
            { id = listboxId instance
            , label = Listbox.noLabel
            , lift = ListboxMsg >> instance.lift
            }

        attributeMap noOp =
            Attributes.map (\_ -> noOp)

        isOpen =
            open config.visibility data entries value

        popup =
            case config.visibility of
                Always empty ->
                    if data.focused && not data.closeRequested then
                        if List.isEmpty entries then
                            Html.map (\_ -> instance.lift NoOp) empty

                        else
                            listbox

                    else
                        Html.text ""

                Sometimes conditions ->
                    if
                        case conditions.whenMatchingWithMinimalValueLength of
                            Nothing ->
                                False

                            Just minimalLength ->
                                not (List.isEmpty entries)
                                    && (String.length value >= minimalLength)
                                    && data.focused
                                    && not data.closeRequested
                    then
                        listbox

                    else
                        case conditions.whenRequested of
                            Nothing ->
                                Html.text ""

                            Just { checkValue, empty } ->
                                if
                                    checkValue value
                                        && data.showRequested
                                        && data.focused
                                        && not data.closeRequested
                                then
                                    if List.isEmpty entries then
                                        Html.map (\_ -> instance.lift NoOp) empty

                                    else
                                        listbox

                                else
                                    Html.text ""

        listbox =
            Listbox.Unique.view listboxViewConfig
                listboxInstance
                entries
                data.listbox
                data.selection
    in
    Html.div
        ([ Attributes.attribute "role" "combobox"
         , Attributes.attribute "aria-expanded" <|
            if isOpen then
                "true"

            else
                "false"
         , Events.onMouseDown (instance.lift ListboxMousePressed)
         , Events.onMouseUp (instance.lift ListboxMouseReleased)
         , Events.on "click"
            (Decode.at [ "target", "id" ] Decode.string
                |> Decode.andThen
                    (\targetId ->
                        if targetId == inputId instance then
                            Decode.fail "not handling that click here"

                        else
                            Decode.succeed (instance.lift ListboxMouseClicked)
                    )
            )
         ]
            |> appendAttributes (attributeMap (instance.lift NoOp)) config.views.container
        )
        [ Html.input
            ([ Attributes.id (inputId instance)
             , Attributes.type_ "text"
             , Attributes.attribute "role" <|
                case config.role of
                    Textbox ->
                        "textbox"

                    Searchbox ->
                        "searchbox"
             , Attributes.attribute "aria-multiline" "false"
             , Attributes.attribute "aria-autocomplete" <|
                case config.visibility of
                    Always _ ->
                        "none"

                    Sometimes _ ->
                        "list"
             , Attributes.value value
             , Listbox.preventDefaultOnKeyDown listboxInstance
                (KeyInfo.decoder
                    |> Decode.andThen
                        (\keyInfo ->
                            case keyInfo.code of
                                "ArrowUp" ->
                                    if
                                        keyInfo.altDown
                                            && not
                                                (keyInfo.controlDown
                                                    || keyInfo.metaDown
                                                    || keyInfo.shiftDown
                                                )
                                    then
                                        Decode.succeed
                                            ( instance.lift
                                                (InputAltArrowUpPressed config.visibility)
                                            , True
                                            )

                                    else
                                        Decode.fail "not handling that key here"

                                "ArrowDown" ->
                                    if
                                        keyInfo.altDown
                                            && not
                                                (keyInfo.controlDown
                                                    || keyInfo.metaDown
                                                    || keyInfo.shiftDown
                                                )
                                    then
                                        Decode.succeed
                                            ( instance.lift InputAltArrowDownPressed
                                            , True
                                            )

                                    else
                                        Decode.fail "not handling that key here"

                                "Escape" ->
                                    Decode.succeed
                                        ( instance.lift InputEscapePressed
                                        , True
                                        )

                                "Enter" ->
                                    Decode.succeed
                                        ( instance.lift InputEnterPressed
                                        , True
                                        )

                                "Alt" ->
                                    Decode.fail "not handling that key here"

                                "Control" ->
                                    Decode.fail "not handling that key here"

                                "Meta" ->
                                    Decode.fail "not handling that key here"

                                "Shift" ->
                                    Decode.fail "not handling that key here"

                                _ ->
                                    Decode.succeed
                                        ( instance.lift InputOtherKeyPressed
                                        , False
                                        )
                        )
                )
             , Events.onInput (instance.lift << ValueChanged)
             , Events.onFocus (instance.lift InputFocused)
             , Events.onBlur (instance.lift InputBlured)
             ]
                |> addAriaControls
                |> addAriaLabelledBy
                |> addAriaActivedescendant
                |> appendAttributes (attributeMap (instance.lift NoOp)) config.views.input
            )
            []
        , popup
        ]


listboxId : Instance a msg -> String
listboxId { id } =
    id ++ "-listbox"


inputId : Instance a msg -> String
inputId { id } =
    id


appendAttributes :
    (attributeNever -> attribute)
    -> List attributeNever
    -> List attribute
    -> List attribute
appendAttributes attributeFromNever neverAttrs attrs =
    neverAttrs
        |> List.map attributeFromNever
        |> List.append attrs



---- UPDATE


{-| TODO
-}
type Msg a
    = NoOp
    | InputFocused
    | InputBlured
    | InputAltArrowUpPressed Visibility
    | InputAltArrowDownPressed
    | InputEscapePressed
    | InputEnterPressed
    | InputOtherKeyPressed
    | ValueChanged String
    | ListboxMsg (Listbox.Msg a)
    | ListboxMousePressed
    | ListboxMouseReleased
    | ListboxMouseClicked


{-| TODO
-}
update :
    UpdateConfig a
    -> List (Entry a divider)
    -> Msg a
    -> Combobox a
    -> String
    -> ( Combobox a, Cmd (Msg a), String )
update (UpdateConfig uniqueId behaviour) entries msg ((Combobox data) as combobox) value =
    case msg of
        NoOp ->
            ( combobox, Cmd.none, value )

        InputFocused ->
            ( Combobox
                { data
                    | focused = True
                    , showRequested = False
                    , closeRequested = False
                }
            , Cmd.none
            , value
            )

        InputBlured ->
            if data.preventBlur then
                ( combobox, Cmd.none, value )

            else
                ( Combobox { data | focused = False }
                , Cmd.none
                , value
                )

        InputAltArrowUpPressed visibility ->
            if open visibility data entries value then
                if
                    Listbox.focusedEntry
                        (listboxUpdateConfig uniqueId behaviour)
                        entries
                        data.listbox
                        == Nothing
                then
                    ( Combobox
                        { data
                            | listbox = Listbox.init
                            , selection = Nothing
                            , showRequested = False
                            , closeRequested = True
                        }
                    , Cmd.none
                    , value
                    )

                else
                    ( Combobox
                        { data
                            | listbox = Listbox.init
                            , selection = Nothing
                        }
                    , Cmd.none
                    , value
                    )

            else
                ( combobox, Cmd.none, value )

        InputAltArrowDownPressed ->
            ( Combobox { data | closeRequested = False }
            , Cmd.none
            , value
            )

        InputEscapePressed ->
            ( Combobox { data | focused = False }
            , Cmd.none
            , if behaviour.clearOnEscape then
                ""

              else
                value
            )

        InputEnterPressed ->
            case data.selection of
                Nothing ->
                    ( combobox, Cmd.none, value )

                Just a ->
                    ( Combobox
                        { data
                            | listbox = Listbox.init
                            , selection = Nothing
                            , showRequested = False
                            , closeRequested = True
                        }
                    , Cmd.none
                    , behaviour.entryToValue a
                    )

        InputOtherKeyPressed ->
            ( Combobox
                { data
                    | listbox = Listbox.init
                    , selection = Nothing
                    , closeRequested = False
                }
            , Cmd.none
            , value
            )

        ValueChanged newValue ->
            ( Combobox { data | closeRequested = False }
            , Cmd.none
            , newValue
            )

        ListboxMsg listboxMsg ->
            let
                ( newListbox, listboxCmd, newSelection ) =
                    Listbox.Unique.update (listboxUpdateConfig uniqueId behaviour)
                        entries
                        listboxMsg
                        data.listbox
                        data.selection
            in
            ( Combobox
                { data
                    | listbox = newListbox
                    , selection = newSelection
                }
            , Cmd.map ListboxMsg listboxCmd
            , value
            )

        ListboxMousePressed ->
            ( Combobox { data | preventBlur = True }
            , Cmd.none
            , value
            )

        ListboxMouseReleased ->
            ( Combobox { data | preventBlur = False }
            , Cmd.none
            , value
            )

        ListboxMouseClicked ->
            case data.selection of
                Nothing ->
                    ( combobox, Cmd.none, value )

                Just a ->
                    ( Combobox
                        { data
                            | listbox = Listbox.init
                            , selection = Nothing
                            , showRequested = False
                            , closeRequested = True
                        }
                    , Cmd.none
                    , behaviour.entryToValue a
                    )


listboxUpdateConfig : (a -> String) -> Behaviour a -> Listbox.UpdateConfig a
listboxUpdateConfig uniqueId behaviour =
    Listbox.updateConfig uniqueId
        { jumpAtEnds = False
        , separateFocus = behaviour.separateFocus
        , selectionFollowsFocus = True
        , handleHomeAndEnd = behaviour.handleHomeAndEnd
        , typeAhead = Listbox.noTypeAhead
        , minimalGap = behaviour.minimalGap
        , initialGap = behaviour.initialGap
        }



---- SUBSCRIPTIONS


{-| TODO
-}
subscriptions : Combobox a -> Sub (Msg a)
subscriptions (Combobox data) =
    Sub.map ListboxMsg (Listbox.subscriptions data.listbox)



---- CONFIGURATION


{-| TODO
-}
type ViewConfig a divider
    = ViewConfig
        { uniqueId : a -> String
        , role : Role
        , visibility : Visibility
        , views : Views a divider
        }


{-| TODO
-}
viewConfig :
    { uniqueId : a -> String
    , role : Role
    , visibility : Visibility
    , views : Views a divider
    }
    -> ViewConfig a divider
viewConfig =
    ViewConfig


{-| TODO
-}
type Role
    = Textbox
    | Searchbox


{-| TODO
-}
type Visibility
    = Always (Html Never)
    | Sometimes
        { whenMatchingWithMinimalValueLength : Maybe Int

        --  TODO
        --, whenMatchingOnValueChange : Bool
        , whenRequested :
            Maybe
                { checkValue : String -> Bool
                , empty : Html Never
                }
        }


{-| TODO
-}
type alias Views a divider =
    { container : HtmlAttributes
    , input : HtmlAttributes
    , ul : HtmlAttributes
    , liOption :
        { selected : Bool
        , focused : Bool
        , hovered : Bool
        , maybeQuery : Maybe String
        }
        -> a
        -> HtmlDetails
    , liDivider : divider -> HtmlDetails
    }


type alias HtmlAttributes =
    List (Attribute Never)


type alias HtmlDetails =
    { attributes : List (Attribute Never)
    , children : List (Html Never)
    }


{-| TODO
-}
type UpdateConfig a
    = UpdateConfig (a -> String) (Behaviour a)


{-| TODO
-}
updateConfig : (a -> String) -> Behaviour a -> UpdateConfig a
updateConfig =
    UpdateConfig


{-| TODO
-}
type alias Behaviour a =
    { entryToValue : a -> String
    , clearOnEscape : Bool
    , separateFocus : Bool
    , handleHomeAndEnd : Bool
    , minimalGap : Float
    , initialGap : Float
    }



---- HELPER


open : Visibility -> ComboboxData a -> List (Entry a divider) -> String -> Bool
open visibility data entries value =
    case visibility of
        Always empty ->
            data.focused
                && not data.closeRequested

        Sometimes conditions ->
            (case conditions.whenMatchingWithMinimalValueLength of
                Nothing ->
                    False

                Just minimalLength ->
                    not (List.isEmpty entries)
                        && (String.length value >= minimalLength)
                        && data.focused
                        && not data.closeRequested
            )
                || (case conditions.whenRequested of
                        Nothing ->
                            False

                        Just { checkValue, empty } ->
                            checkValue value
                                && data.showRequested
                                && data.focused
                                && not data.closeRequested
                   )
