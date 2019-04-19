module Listbox.Combobox exposing
    ( view, Instance
    , Combobox, init
    , update, Msg, subscriptions
    , UpdateConfig, updateConfig, Behaviour
    , ViewConfig, viewConfig, Role(..), Visibility(..), Views
    , customView
    , DomFunctions
    , CustomViewConfig, customViewConfig, CustomVisibility(..), CustomViews
    )

{-| Implementation of the [combobox
widget](https://www.w3.org/TR/wai-aria-practices-1.1/#combobox):

> A combobox is a widget made up of the combination of two distinct elements:
>
> 1.  a single-line textbox, and
> 2.  an associated pop-up element for helping users set the value of the textbox.

In this module the pop-up is a listbox. Take a look at the documentation of
`Behaviour` for the default keyboard interactions this widget offers.


# View

@docs view, Instance


# State

@docs Combobox, init


# Update

@docs update, Msg, subscriptions


# Configuration


## Update

@docs UpdateConfig, updateConfig, Behaviour


## View

@docs ViewConfig, viewConfig, Role, Visibility, Views


## Using different DOM libraries

You can use these functions if you want to use other DOM libraries, like for
example `rtfeldman/elm-css` or `mdgriffith/elm-ui`.

@docs customView

@docs DomFunctions

@docs CustomViewConfig, customViewConfig, CustomVisibility, CustomViews

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
import Internal.Label exposing (Label(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
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
view config =
    customView htmlFunctions (viewConfigToCustom config)


htmlFunctions : DomFunctions (Attribute msg) (Attribute Never) (Html msg) (Html Never) msg
htmlFunctions =
    { div = Html.div
    , text = Html.text
    , input = Html.input
    , ul = Html.ul
    , li = Html.li
    , on = Events.on
    , onInput = Events.onInput
    , preventDefaultOn = Events.preventDefaultOn
    , property = Attributes.property
    , attribute = Attributes.attribute
    , style = Attributes.style
    , attributeMap = \noOp -> Attributes.map (\_ -> noOp)
    , htmlMap = \noOp -> Html.map (\_ -> noOp)
    }


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
    | InputAltArrowUpPressed Bool
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

        InputAltArrowUpPressed isOpen ->
            if isOpen then
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



---- CUSTOM DOM


{-| This record holds all the DOM functions needed to render a listbox. It is
probably instructive to look at the version for the standard `elm/html`
package:

    htmlFunctions : DomFunctions (Attribute msg) (Attribute Never) (Html msg) (Html Never) msg
    htmlFunctions =
        { div = Html.div
        , text = Html.text
        , input = Html.input
        , ul = Html.ul
        , li = Html.li
        , on = Events.on
        , onInput = Events.onInput
        , preventDefaultOn = Events.preventDefaultOn
        , property = Attributes.property
        , attribute = Attributes.attribute
        , style = Attributes.style
        , attributeMap = \noOp -> Attributes.map (\_ -> noOp)
        , htmlMap = \noOp -> Html.map (\_ -> noOp)
        }

When using `mdgriffith/elm-ui`, you could define something like this:

    elementFunctions : DomFunctions (Attribute msg) (Attribute Never) (Element msg) (Element Never) msg
    elementFunctions =
        { div = Element.column
        , text = Element.text
        , input = TODO
        , ul = Element.column
        , li = Element.row
        , on = Element.htmlAttribute (Events.on event decoder)
        , onInput =
            \tagger ->
                Element.htmlAttribute (Events.onInput tagger)
        , preventDefaultOn =
            \event decoder ->
                Element.htmlAttribute (Events.preventDefaultOn event decoder)
        , property =
            \name value -> Element.htmlAttribute (Attributes.property name value)
        , attribute =
            \name value ->
                Element.htmlAttribute (Attributes.attribute name value)
        , style =
            \name value ->
                Element.htmlAttribute (Attributes.style name value)
        , attributeMap = \noOp -> Element.mapAttribute (\_ -> noOp)
        , htmlMap = \noOp -> Element.map (\_ -> noOp)
        }

-}
type alias DomFunctions attribute attributeNever html htmlNever msg =
    { div : List attribute -> List html -> html
    , text : String -> htmlNever
    , input : List attribute -> List html -> html
    , ul : List attribute -> List html -> html
    , li : List attribute -> List html -> html
    , on : String -> Decoder msg -> attribute
    , onInput : (String -> msg) -> attribute
    , preventDefaultOn : String -> Decoder ( msg, Bool ) -> attribute
    , property : String -> Value -> attribute
    , attribute : String -> String -> attribute
    , style : String -> String -> attributeNever
    , attributeMap : msg -> attributeNever -> attribute
    , htmlMap : msg -> htmlNever -> html
    }


{-| -}
type CustomViewConfig a divider attributeNever htmlNever
    = CustomViewConfig
        { uniqueId : a -> String
        , role : Role
        , visibility : CustomVisibility htmlNever
        , views : CustomViews a divider attributeNever htmlNever
        }


{-| A replacement for `viewConfig` when you are using your own `customView`
function.
-}
customViewConfig :
    { uniqueId : a -> String
    , role : Role
    , visibility : CustomVisibility htmlNever
    , views : CustomViews a divider attributeNever htmlNever
    }
    -> CustomViewConfig a divider attributeNever htmlNever
customViewConfig =
    CustomViewConfig


viewConfigToCustom :
    ViewConfig a divider
    -> CustomViewConfig a divider (Attribute Never) (Html Never)
viewConfigToCustom (ViewConfig config) =
    CustomViewConfig
        { uniqueId = config.uniqueId
        , role = config.role
        , visibility = visibilityToCustom config.visibility
        , views = config.views
        }


{-| -}
type CustomVisibility htmlNever
    = CustomAlways htmlNever
    | CustomSometimes
        { whenMatchingWithMinimalValueLength : Maybe Int

        --  TODO
        --, whenMatchingOnValueChange : Bool
        , whenRequested :
            Maybe
                { checkValue : String -> Bool
                , empty : htmlNever
                }
        }


visibilityToCustom : Visibility -> CustomVisibility (Html Never)
visibilityToCustom visibility =
    case visibility of
        Always empty ->
            CustomAlways empty

        Sometimes conditions ->
            CustomSometimes
                { whenMatchingWithMinimalValueLength =
                    conditions.whenMatchingWithMinimalValueLength
                , whenRequested = conditions.whenRequested
                }


{-| A replacement for `Views` when you are using your own `customView`
function. Take a look at its documentation for a description of each field.
-}
type alias CustomViews a divider attributeNever htmlNever =
    { container : DomAttributes attributeNever
    , input : DomAttributes attributeNever
    , ul : DomAttributes attributeNever
    , liOption :
        { selected : Bool
        , focused : Bool
        , hovered : Bool
        , maybeQuery : Maybe String
        }
        -> a
        -> DomDetails attributeNever htmlNever
    , liDivider : divider -> DomDetails attributeNever htmlNever
    }


type alias DomAttributes attributeNever =
    List attributeNever


type alias DomDetails attributeNever htmlNever =
    { attributes : List attributeNever
    , children : List htmlNever
    }


{-| Create a customized view function for the DOM library of your choice by
providing some `DomFunctions`.
-}
customView :
    DomFunctions attribute attributeNever html htmlNever msg
    -> CustomViewConfig a divider attributeNever htmlNever
    -> Instance a msg
    -> List (Entry a divider)
    -> Combobox a
    -> String
    -> html
customView dom (CustomViewConfig config) instance entries (Combobox data) value =
    let
        addAriaControls attrs =
            if isOpen then
                dom.attribute "aria-controls" (listboxId instance) :: attrs

            else
                attrs

        addAriaLabelledBy attrs =
            case instance.label of
                LabelledBy id ->
                    dom.attribute "aria-labelledby" id :: attrs

                Label id ->
                    dom.attribute "aria-label" id :: attrs

                NoLabel ->
                    attrs

        addAriaActivedescendant attrs =
            case
                Listbox.customFocusedEntryId listboxViewConfig
                    listboxInstance
                    entries
                    data.listbox
            of
                Nothing ->
                    attrs

                Just id ->
                    dom.attribute "aria-activedescendant" id :: attrs

        listboxDom =
            { ul = dom.ul
            , li = dom.li
            , property = dom.property
            , attribute = dom.attribute
            , on = dom.on
            , preventDefaultOn = dom.preventDefaultOn
            , attributeMap = dom.attributeMap
            , htmlMap = dom.htmlMap
            }

        listboxViewConfig =
            Listbox.customViewConfig
                { uniqueId = config.uniqueId
                , views =
                    { ul =
                        if isOpen then
                            dom.style "position" "absolute" :: config.views.ul

                        else
                            dom.style "display" "none"
                                :: dom.style "position" "absolute"
                                :: config.views.ul
                    , liOption = config.views.liOption
                    , liDivider = config.views.liDivider
                    , empty = dom.text ""
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
                CustomAlways empty ->
                    if data.focused && not data.closeRequested then
                        if List.isEmpty entries then
                            dom.htmlMap (instance.lift NoOp) empty

                        else
                            listbox

                    else
                        dom.htmlMap (instance.lift NoOp) empty

                CustomSometimes conditions ->
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
                                dom.htmlMap (instance.lift NoOp) (dom.text "")

                            Just { checkValue, empty } ->
                                if
                                    checkValue value
                                        && data.showRequested
                                        && data.focused
                                        && not data.closeRequested
                                then
                                    if List.isEmpty entries then
                                        dom.htmlMap (instance.lift NoOp) empty

                                    else
                                        listbox

                                else
                                    dom.htmlMap (instance.lift NoOp) (dom.text "")

        listbox =
            Listbox.customViewUnique listboxDom
                listboxViewConfig
                listboxInstance
                entries
                data.listbox
                data.selection
    in
    dom.div
        ([ dom.attribute "role" "combobox"
         , dom.attribute "aria-expanded" <|
            if isOpen then
                "true"

            else
                "false"
         , dom.on "mousedown" (Decode.succeed (instance.lift ListboxMousePressed))
         , dom.on "mouseup" (Decode.succeed (instance.lift ListboxMouseReleased))
         , dom.on "click"
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
            |> appendAttributes (dom.attributeMap (instance.lift NoOp)) config.views.container
        )
        [ dom.input
            ([ dom.property "id" (Encode.string (inputId instance))
             , dom.property "type" (Encode.string "text")
             , dom.attribute "role" <|
                case config.role of
                    Textbox ->
                        "textbox"

                    Searchbox ->
                        "searchbox"
             , dom.attribute "aria-multiline" "false"
             , dom.attribute "aria-autocomplete" <|
                case config.visibility of
                    CustomAlways _ ->
                        "none"

                    CustomSometimes _ ->
                        "list"
             , dom.property "value" (Encode.string value)
             , Listbox.customPreventDefaultOnKeyDown dom.preventDefaultOn
                listboxInstance
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
                                        -- TODO: this may not work in some
                                        -- browsers/on some platforms, due to
                                        -- https://github.com/elm/html/issues/180
                                        Decode.succeed
                                            ( instance.lift
                                                (InputAltArrowUpPressed isOpen)
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
             , dom.onInput (instance.lift << ValueChanged)
             , dom.on "focus" (Decode.succeed (instance.lift InputFocused))
             , dom.on "blur" (Decode.succeed (instance.lift InputBlured))
             ]
                |> addAriaControls
                |> addAriaLabelledBy
                |> addAriaActivedescendant
                |> appendAttributes (dom.attributeMap (instance.lift NoOp)) config.views.input
            )
            []
        , popup
        ]



---- HELPER


open :
    CustomVisibility htmlNever
    -> ComboboxData a
    -> List (Entry a divider)
    -> String
    -> Bool
open visibility data entries value =
    case visibility of
        CustomAlways empty ->
            data.focused
                && not data.closeRequested

        CustomSometimes conditions ->
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
