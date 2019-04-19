module Listbox.Combobox exposing
    ( Combobox, init, view, Instance, Label, label, labelledBy, noLabel
    , update, Msg, subscriptions
    , UpdateConfig, updateConfig, Behaviour
    , ViewConfig, viewConfig, Role(..), Visibility(..), Views
    )

{-| Implementation of the [combobox
widget](https://www.w3.org/TR/wai-aria-practices-1.1/#combobox):

> A combobox is a widget made up of the combination of two distinct elements:
>
> 1.  a single-line textbox, and
> 2.  an associated pop-up element for helping users set the value of the textbox.

In this module the pop-up is a listbox. Take a look at the documentation of `Behaviour` for the default keyboard
interactions this widget offers.

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


{-| Tracks the keyboard and mouse focus in the pop-up. The value of the textbox
as well as the (filtered) list of entries live in your own model.
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


{-| An initial combobox with no option focused.
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


{-| To make a listbox unique in your application you have to provide this
information to the `view` function:

  - **id**: The unique id of the listbox.

  - **label**: Specify how the combobox is labelled. See `Label` for
    possible options.

  - **lift**: Your message type constructor wrapping the listbox `Msg`'s.

-}
type alias Instance a msg =
    { id : String
    , label : Label
    , lift : Msg a -> msg
    }


{-| There are three possibilities to label a combox: it can be
`labelledBy` by another DOM element with the given id, it can provide its own
`label`, or it can have `noLabel` at all.

The last case is only allowed when the combobox is part of another widget which
itself is labelled.

-}
type Label
    = LabelledBy String
    | Label String
    | NoLabel


{-| -}
labelledBy : String -> Label
labelledBy =
    LabelledBy


{-| -}
label : String -> Label
label =
    Label


{-| -}
noLabel : Label
noLabel =
    NoLabel


{-| Take a list of all entries and the value of the textbox and display it as
a combobox. Note, that you have to filter the entries yourself. You have to
provide a `ViewConfig` for the styling and an `Instance` to uniquely identify
this combobox. For example:

    view : Combobox -> String -> Html Msg
    view combobox value =
        let
            matchingFruits =
                fruits
                    |> List.filter (String.contains value)
                    |> List.map Listbox.option
        in
        Html.div []
            [ Combobox.view viewConfig
                { id = "fruits-combobox"
                , label = label "fruits"
                , lift = ComboboxMsg
                }
                matchingFruits
                combobox
                value
            ]

    fruits : List String
    fruits =
        [ "Apple", "Banana", "Cherry" ]

    type Msg
        = ComboboxMsg Combobox.Msg

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
            Listbox.viewConfig
                { uniqueId = config.uniqueId
                , views =
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
            Listbox.viewUnique listboxViewConfig
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


{-| The combobox's message type.
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


{-| Use this function to update the combobox state. You have to provide the
same entries and value as given to your view function. Note, that you have to
filter the entries yourself.

For example:

    update msg model =
        case msg of
            ComboboxMsg comboboxMsg ->
                let
                    ( newCombobox, comboboxCmd, newValue ) =
                        Combobox.update updateConfig
                            matchingFruits
                            comboboxMsg
                            model.combobox
                            model.value

                    matchingFruits =
                        fruits
                            |> List.filter
                                (String.contains model.value)
                            |> List.map Listbox.option
                in
                ( { model
                    | combobox = newCombobox
                    , value = newValue
                  }
                , Cmd.map ComboboxMsg comboboxCmd
                )

In a more sophisticated example, the entries could be dynamic, as well. (For
example, loaded via an HTTP request.)

-}
update :
    UpdateConfig a
    -> List (Entry a divider)
    -> Msg a
    -> Combobox a
    -> String
    -> ( Combobox a, Cmd (Msg a), String )
update (UpdateConfig cfg) entries msg ((Combobox data) as combobox) value =
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
                        (listboxUpdateConfig cfg.uniqueId cfg.behaviour)
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
            , if cfg.behaviour.clearOnEscape then
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
                    , cfg.behaviour.entryToValue a
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
                    Listbox.updateUnique (listboxUpdateConfig cfg.uniqueId cfg.behaviour)
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
                    , cfg.behaviour.entryToValue a
                    )


listboxUpdateConfig : (a -> String) -> Behaviour a -> Listbox.UpdateConfig a
listboxUpdateConfig uniqueId behaviour =
    Listbox.updateConfig
        { uniqueId = uniqueId
        , behaviour =
            { jumpAtEnds = False
            , separateFocus = behaviour.separateFocus
            , selectionFollowsFocus = True
            , handleHomeAndEnd = behaviour.handleHomeAndEnd
            , typeAhead = Listbox.noTypeAhead
            , minimalGap = behaviour.minimalGap
            , initialGap = behaviour.initialGap
            }
        }



---- SUBSCRIPTIONS


{-| Do not forget to add this to your subscriptions:

    subscriptions model =
        Sub.map ComboboxMsg
            (Combobox.subscriptions model.combobox)

-}
subscriptions : Combobox a -> Sub (Msg a)
subscriptions (Combobox data) =
    Sub.map ListboxMsg (Listbox.subscriptions data.listbox)



---- CONFIGURATION


{-| -}
type ViewConfig a divider
    = ViewConfig
        { uniqueId : a -> String
        , role : Role
        , visibility : Visibility
        , views : Views a divider
        }


{-| Generate a `ViewConfig` by providing the following:

  - **uniqueId**: A hash function for the entries.

  - **role**: The role of the textbox.

  - **visibility**: When should the pop-up be displayed?

  - **views**: View customizations.

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


{-| The textbox of the combobox can have one of these two roles.
-}
type Role
    = Textbox
    | Searchbox


{-| Provided the list of entries is not empty, when should the pop-up be
visible? Either `Always` or `Sometimes` when at least one of the following
conditions are met:

  - **whenMatchingWithMinimalValueLength**: The value has a minimum length.

  - **whenRequested**: The display was explicitely requested by the user, for
    example by pressing `Alt + Down`. Also, `checkValue` which is given the
    current value, must evaluate to `True`.

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


{-| **Available view customizations**

This is part of the arguments to `viewConfig`. You can customize the styling
with the following fields:

  - **container**: A list of html attributes applied to the container div which
    holds the combobox.

  - **input**: A list of html attributes applied to the textbox.

  - **ul**: A list of html attributes applied to the pop-up listbox.

  - **liOption**: A function returning `HtmlDetails` for each option in your
    entries list. It gets the actual option value `a` and flags telling you if
    this option is currently `selected`, `focused` or `hovered`. If the user
    typed in a query, you get this via the `maybeQuery` field.

  - **liDivider**: This lets you style the divider list entries. It gets the
    actual `divider` entry and returns `HtmlDetails`.

The DOM structure of a combobox with visible pop-up will be something like
this:

    comboboxOpen =
        Html.div
            [ ... ] -- container attributes
            [ Html.input
                [ ... ] -- input attributes
                []
            , Html.ul
                [ ... ] -- ul attributes
                [ Html.li
                    [ ... ] -- liDivider attributes
                    [ ... ] -- liDivider children
                , Html.li
                    [ ... ] -- liOption attributes
                    [ ... ] -- liOption children
                , ...
                , Html.li
                    [ ... ] -- liOption attributes
                    [ ... ] -- liOption children
                ]
            ]

When the pop-up is not visible, it will look like this:

    comboboxClosed =
        Html.div
            [ ... ] -- container attributes
            [ Html.input
                [ ... ] -- input attributes
                []
            ]

Provided you have specified some CSS classes, a view configuration could look
like this:

    views : Views String Never
    views =
        { container =
            [ Html.Attributes.class "combobox__container" ]
        , input = [ Html.Attributes.class "combobox__input" ]
        , ul = [ Html.Attributes.class "listbox__container" ]
        , liOption =
            \{ selected, focused } option ->
                { attributes =
                    [ Html.Attributes.class "listbox__option"
                    , Html.Attributes.classList
                        [ ( "listbox__option--selected"
                          , selected
                          )
                        , ( "listbox__option--keyboardFocused"
                          , focused
                          )
                        ]
                    ]
                , children =
                    [ Html.text option ]
                }
        , liDivider = noDivider
        }

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


{-| -}
type UpdateConfig a
    = UpdateConfig
        { uniqueId : a -> String
        , behaviour : Behaviour a
        }


{-| Generate an `UpdateConfig` by providing the following:

  - **uniqueId**: A hash function for the entries.

  - **behaviour**: Behaviour customizations.

-}
updateConfig :
    { uniqueId : a -> String
    , behaviour : Behaviour a
    }
    -> UpdateConfig a
updateConfig =
    UpdateConfig


{-| **Available behaviour customizations**

You can customize the behaviour of the combobox with the following options:

  - **entryToValue**: How to transform an entry into a `String`. This will be
    the new value when an entry gets selected in the listbox.

  - **clearOnEscape**: Should the textbox be cleared when the pop-up is
    dismissed by pressing Escape?

  - **separateFocus**: Whether the mouse focus and the keyboard focus can be
    different.

  - **handleHomeAndEnd**: Should we handle the `Home` and `End` keys (to jump
    to the top or bottom of the list)?

  - **minimalGap**: If the distance (in px) of the option having the keyboard
    focus to the borders of the listbox's viewport is smaller then this value,
    the listbox will adjust its scroll position so that this distance is at least
    `initialGap`.

  - **initialGap**: The minimal distance (in px) of the option having the
    keyboard focus to the borders of the listbox's viewport after the scroll
    position has been adjusted.

A behaviour configuration could look something like this:

    behaviour : Behaviour String
    behaviour =
        { entryToValue = identity
        , clearOnEscape = False
        , separateFocus = True
        , handleHomeAndEnd = True
        , minimalGap = 30
        , initialGap = 200
        }

The listbox will behave as explained in the [WAI-ARIA Authoring Practices
1.1](https://www.w3.org/TR/wai-aria-practices-1.1/#combobox) in the _Keyboard
Interaction_ section.

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
