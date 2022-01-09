module Listbox exposing
    ( view, Instance, Label, labelledBy, label, noLabel
    , Listbox, init
    , update, Msg, subscriptions
    , UpdateConfig, updateConfig, Behaviour
    , ViewConfig, viewConfig, Views, noDivider
    , HtmlAttributes, HtmlDetails
    , TypeAhead, noTypeAhead, simpleTypeAhead, typeAhead
    , viewUnique, updateUnique
    , focusEntryUnique, focusNextOrFirstEntryUnique, focusPreviousOrFirstEntryUnique
    , focusedEntry, focusedEntryId, hoveredEntry
    , focusEntry, focusNextOrFirstEntry, focusPreviousOrFirstEntry
    , focus
    , scrollToFocus
    , preventDefaultOnKeyDown
    , html, custom
    )

{-| Implementation of the [listbox
widget](https://www.w3.org/TR/wai-aria-practices-1.1/#Listbox):

> A listbox widget presents a list of options and allows a user to select one
> or more of them.

Take a look at the documentation of `Behaviour` for the default keyboard
interactions this widget offers.


# View

@docs view, Instance, Label, labelledBy, label, noLabel


# State

@docs Listbox, init


# Update

@docs update, Msg, subscriptions


# Configuration


## Update

@docs UpdateConfig, updateConfig, Behaviour


## View

@docs ViewConfig, viewConfig, Views, noDivider

@docs HtmlAttributes, HtmlDetails


## Type-ahead

@docs TypeAhead, noTypeAhead, simpleTypeAhead, typeAhead


# Unique selection

If you need a `Listbox` allowing only **at most one** selection. You just have
to replace the `view`, `update`, ... functions with the following ones in this
section.

@docs viewUnique, updateUnique

@docs focusEntryUnique, focusNextOrFirstEntryUnique, focusPreviousOrFirstEntryUnique


# Advanced usage


## State info

@docs focusedEntry, focusedEntryId, hoveredEntry


## State manipulation

@docs focusEntry, focusNextOrFirstEntry, focusPreviousOrFirstEntry


## DOM Stuff

@docs focus

@docs scrollToFocus

@docs preventDefaultOnKeyDown


## Using different DOM libraries

You can use these functions if you want to use other DOM libraries, like for
example `rtfeldman/elm-css` or `mdgriffith/elm-ui`.

@docs html, custom

-}

{-

   Copyright 2018 Fabian Kirchner

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

import Accessibility.Aria as Aria
import Accessibility.Role as Role
import Accessibility.Widget as Widget
import Browser.Dom as Dom
import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Internal.KeyInfo as KeyInfo exposing (KeyInfo)
import Internal.Label as Internal exposing (Label(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode exposing (Value)
import List.Extra as List
import Set
import Task exposing (Task)
import Time exposing (Posix)


{-| Tracks the keyboard and mouse focus as well as the current query. The full
list of entries and the currently selected option(s) live in your own model.
-}
type Listbox
    = Listbox
        { preventScroll : Bool
        , query : Query

        -- FOCUS
        , focus : Focus
        , hover : Maybe String
        , maybeLastSelectedEntry : Maybe String
        }


type Query
    = NoQuery
    | Query Int Time.Posix String


type Focus
    = NoFocus
    | Focus String
    | Pending
        { id : String
        , current : String
        , pending : String
        , shiftDown : Bool
        }


currentFocus : Focus -> Maybe String
currentFocus theFocus =
    case theFocus of
        NoFocus ->
            Nothing

        Focus current ->
            Just current

        Pending { current } ->
            Just current


{-| An initial listbox with no option focused.
-}
init : Listbox
init =
    Listbox
        { preventScroll = False
        , query = NoQuery
        , focus = NoFocus
        , hover = Nothing
        , maybeLastSelectedEntry = Nothing
        }



---- EXTERNAL STATE MANIPULATION


{-| A task to give the listbox focus. The first argument must match the
`Instance` used in the `view` function!
-}
focus : Instance a msg -> Task Dom.Error ()
focus { id } =
    Dom.focus (printListId id)


{-| Returns the option which currently has keyboard focus.
-}
focusedEntry : UpdateConfig a -> List a -> Listbox -> Maybe a
focusedEntry (UpdateConfig config) allEntries (Listbox listbox) =
    Maybe.andThen (find config.uniqueId allEntries) (currentFocus listbox.focus)


{-| Returns the option which currently has mouse focus.
-}
hoveredEntry : UpdateConfig a -> List a -> Listbox -> Maybe a
hoveredEntry (UpdateConfig config) allEntries (Listbox listbox) =
    Maybe.andThen (find config.uniqueId allEntries) listbox.hover


{-| Sets the keyboard focus to the provided options.

**Note**: This will not adjust the scroll position of the listbox, so you might
want to apply `scrollToFocus` afterwards.

-}
focusEntry : UpdateConfig a -> a -> Listbox -> List a -> ( Listbox, List a )
focusEntry (UpdateConfig config) newEntry (Listbox listbox) selection =
    ( Listbox
        { listbox
            | query = NoQuery
            , focus = Focus (config.uniqueId newEntry)
        }
    , if config.behaviour.selectionFollowsFocus then
        [ newEntry ]

      else
        selection
    )


{-| Sets the keyboard focus to the next option. If `jumpAtEnds` is true and the
focus is already on the last option, the first option is selected.

**Note**: This will not adjust the scroll position of the listbox, so you might
want to apply `scrollToFocus` afterwards.

-}
focusNextOrFirstEntry :
    UpdateConfig a
    -> List a
    -> Listbox
    -> List a
    -> ( Listbox, List a )
focusNextOrFirstEntry (UpdateConfig config) allEntries (Listbox listbox) selection =
    let
        { uniqueId, behaviour } =
            config

        maybeA =
            case currentFocus listbox.focus of
                Nothing ->
                    List.head allEntries

                Just hash ->
                    case findNext uniqueId allEntries hash of
                        Just (First a) ->
                            if behaviour.jumpAtEnds then
                                Just a

                            else
                                Nothing

                        Just (Next a) ->
                            Just a

                        Nothing ->
                            Nothing
    in
    case maybeA of
        Nothing ->
            ( Listbox listbox, selection )

        Just a ->
            let
                newListbox =
                    Listbox { listbox | focus = Focus (uniqueId a) }
            in
            if behaviour.selectionFollowsFocus then
                ( newListbox
                , [ a ]
                )

            else
                ( newListbox
                , selection
                )


{-| Sets the keyboard focus to the previous option. If `jumpAtEnds` is true and the
focus is already on the first option, the first option is selected.

**Note**: This will not adjust the scroll position of the listbox, so you might
want to apply `scrollToFocus` afterwards.

-}
focusPreviousOrFirstEntry :
    UpdateConfig a
    -> List a
    -> Listbox
    -> List a
    -> ( Listbox, List a )
focusPreviousOrFirstEntry (UpdateConfig config) allEntries (Listbox listbox) selection =
    let
        { uniqueId, behaviour } =
            config

        maybeA =
            case currentFocus listbox.focus of
                Nothing ->
                    List.head allEntries

                Just hash ->
                    case findPrevious uniqueId allEntries hash of
                        Just (Last a) ->
                            if behaviour.jumpAtEnds then
                                Just a

                            else
                                Nothing

                        Just (Previous a) ->
                            Just a

                        Nothing ->
                            Nothing
    in
    case maybeA of
        Nothing ->
            ( Listbox listbox, selection )

        Just a ->
            let
                newListbox =
                    Listbox { listbox | focus = Focus (uniqueId a) }
            in
            if behaviour.selectionFollowsFocus then
                ( newListbox
                , [ a ]
                )

            else
                ( newListbox
                , selection
                )


{-| A command adjusting the scroll position of the listbox such that the
current keyboard focus is visible.
-}
scrollToFocus : Behaviour a -> Instance a msg -> Listbox -> Cmd msg
scrollToFocus behaviour { id, lift } (Listbox listbox) =
    Cmd.map lift
        (perform
            (case listbox.focus of
                NoFocus ->
                    CmdNone

                Focus current ->
                    ScrollToOption behaviour id current Nothing

                Pending { current } ->
                    ScrollToOption behaviour id current Nothing
            )
        )



---- VIEW CONFIG


{-| -}
type ViewConfig a
    = ViewConfig
        { uniqueId : a -> String
        , focusable : Bool
        , markActiveDescendant : Bool
        }


{-| Generate a `ViewConfig` by providing the following:

  - **uniqueId**: A hash function for the entries.

  - **views**: View customizations.

You usually do **not** want to store this inside your model.

-}
viewConfig :
    { uniqueId : a -> String
    , focusable : Bool
    , markActiveDescendant : Bool
    }
    -> ViewConfig a
viewConfig =
    ViewConfig


{-| Helper function which can be used for the `liDivider` field in your view
customizations if you do not have any dividers in your listbox.
-}
noDivider : Never -> HtmlDetails
noDivider _ =
    { attributes = []
    , children = []
    }


{-| A list of attributes which never throw messages. Used to apply styling to
a DOM element.
-}
type alias HtmlAttributes =
    List (Attribute Never)


{-| Used to apply styling and content to a DOM element.
-}
type alias HtmlDetails =
    { attributes : List (Attribute Never)
    , children : List (Html Never)
    }



---- UPDATE CONFIG


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

You can customize the behaviour of the listbox with the following options:

  - **jumpAtEnds**: Whether the keyboard focus should jump to the other end of
    the list when pressing `ArrowUp` while focusing the first option (or
    `ArrowDown` while focusing the last).

  - **separateFocus**: Whether the mouse focus and the keyboard focus can be
    different.

  - **selectionFollowsFocus**: Do we automatically add the entry gaining
    keyboard focus to the selection?

  - **handleHomeAndEnd**: Should we handle the `Home` and `End` keys (to jump
    to the top or bottom of the list)?

  - **typeAhead**: Make it possible to jump to options by typing in a query.
    Take a look at `TypeAhead` for more information.

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
        { jumpAtEnds = True
        , separateFocus = True
        , selectionFollowsFocus = False
        , handleHomeAndEnd = True
        , typeAhead = simpleTypeAhead 300 identity
        , minimalGap = 30
        , initialGap = 200
        }

The listbox will behave as explained in the [WAI-ARIA Authoring Practices
1.1](https://www.w3.org/TR/wai-aria-practices-1.1/#Listbox) in the _Keyboard
Interaction_ section. Note that you get the _recommended selection model_ if
you choose `selectionFollowsFocus = False`, and the _alternative
selection model_ for `selectionFollowsFocus = True`.

-}
type alias Behaviour a =
    { jumpAtEnds : Bool
    , separateFocus : Bool
    , selectionFollowsFocus : Bool
    , handleHomeAndEnd : Bool
    , typeAhead : TypeAhead a
    , minimalGap : Float
    , initialGap : Float
    }


{-| -}
type TypeAhead a
    = NoTypeAhead
    | TypeAhead Int (String -> a -> Bool)


{-| Use this inside `Behaviour` if you do not want to activate the type-ahead
functionality.
-}
noTypeAhead : TypeAhead a
noTypeAhead =
    NoTypeAhead


{-| Activate the type-ahead functionality. When the user types in a search
query.

  - The first argument is the timeout (in milliseconds) after which the query
    is reseted.

  - The second argument, `a -> String`, should be a reasonable stringification
    of the options. It is used to check whether an option starts with this query
    or not. The listbox will then move the keyboard focus forward to the next
    matching option.

-}
simpleTypeAhead : Int -> (a -> String) -> TypeAhead a
simpleTypeAhead timeout entryToString =
    TypeAhead timeout <|
        \query a ->
            String.toLower (entryToString a)
                |> String.startsWith (String.toLower query)


{-| This works like `simpleTypeAhead` but gives you you more flexibility when
customizing the matching condition. The first argument is the timeout. The
second argument is a function which gets the current query and an option,
returning if the query matches this option.
-}
typeAhead : Int -> (String -> a -> Bool) -> TypeAhead a
typeAhead =
    TypeAhead



---- VIEW


{-| To make a listbox unique in your application you have to provide this
information to the `view` function:

  - **id**: The unique id of the listbox.

  - **label**: Specify how the listbox is labelled. See `Label` for
    possible options.

  - **lift**: Your message type constructor wrapping the listbox `Msg`'s.

-}
type alias Instance a msg =
    { id : String
    , label : Label
    , lift : Msg a -> msg
    }


{-| There are three possibilities to label a listbox: it can be
`labelledBy` by another DOM element with the given id, it can provide its own
`label`, or it can have `noLabel` at all.

The last case is only allowed when the listbox is part of another widget which
itself is labelled.

-}
type alias Label =
    Internal.Label


{-| -}
labelledBy : String -> Label
labelledBy =
    Internal.LabelledBy


{-| -}
label : String -> Label
label =
    Internal.Label


{-| -}
noLabel : Label
noLabel =
    Internal.NoLabel


{-| Take a list of all entries and a list of selected options and display it as
a listbox. You have to provide a `ViewConfig` for the styling and an `Instance`
to uniquely identify this listbox. For example:

    view : Listbox -> List String -> Html Msg
    view listbox selection =
        Html.div []
            [ Listbox.view viewConfig
                { id = "fruits-listbox"
                , label = label "fruits"
                , lift = ListboxMsg
                }
                fruits
                listbox
                selection
            ]

    fruits : List (Entry String divider)
    fruits =
        List.map Listbox.option
            [ "Apple", "Banana", "Cherry" ]

    type Msg
        = ListboxMsg Listbox.Msg

-}
view : Views a node msg -> ViewConfig a -> Instance a msg -> List a -> Listbox -> List a -> node
view =
    viewHelp True


viewHelp :
    Bool
    -> Views a node msg
    -> ViewConfig a
    -> Instance a msg
    -> List a
    -> Listbox
    -> List a
    -> node
viewHelp multiSelectable (Views views) (ViewConfig config) instance allEntries (Listbox listbox) selection =
    let
        { lift } =
            instance

        { uniqueId } =
            config

        viewEntryHelp entry =
            let
                maybeHash =
                    Just (uniqueId entry)

                focused =
                    currentFocus listbox.focus == maybeHash

                hovered =
                    listbox.hover == maybeHash

                selected =
                    List.any ((==) entry) selection
            in
            viewEntry views.option
                multiSelectable
                focused
                hovered
                selected
                (ViewConfig config)
                instance
                listbox.query
                entry

        ariaLabelledby =
            case instance.label of
                LabelledBy id ->
                    Just id

                Label _ ->
                    Nothing

                NoLabel ->
                    Nothing

        ariaLabel =
            case instance.label of
                LabelledBy _ ->
                    Nothing

                Label theLabel ->
                    Just theLabel

                NoLabel ->
                    Nothing

        ariaActivedescendant focused =
            if config.markActiveDescendant then
                focused
                    |> Maybe.andThen (find uniqueId allEntries)
                    |> Maybe.map (uniqueId >> printEntryId instance.id)

            else
                Nothing
    in
    views.listbox
        { id = printListId instance.id
        , role = "listbox"
        , ariaMultiselectable = stringFromBool multiSelectable
        , ariaLabelledby = ariaLabelledby
        , ariaActivedescendant = ariaActivedescendant (currentFocus listbox.focus)
        , tabindex =
            if config.focusable then
                Just 0

            else
                Nothing
        , preventDefaultOnKeydown =
            Decode.andThen
                (listKeyPress False instance.id
                    >> Decode.map (\msg -> ( lift msg, True ))
                )
                KeyInfo.decoder
        , onMousedown = lift ListMouseDown
        , onMouseup = lift ListMouseUp
        , onFocus = lift (ListFocused instance.id)
        , onBlur = lift ListBlured
        }
        { options = List.map viewEntryHelp allEntries
        }


stringFromBool : Bool -> String
stringFromBool bool =
    if bool then
        "true"

    else
        "false"


viewEntry :
    (OptionAttrs msg
     ->
        { selected : Bool
        , focused : Bool
        , hovered : Bool
        , maybeQuery : Maybe String
        }
     -> a
     -> node
    )
    -> Bool
    -> Bool
    -> Bool
    -> Bool
    -> ViewConfig a
    -> Instance a msg
    -> Query
    -> a
    -> node
viewEntry viewOption multiSelectable focused hovered selected (ViewConfig config) instance query entry =
    let
        { uniqueId } =
            config

        { id, lift } =
            instance

        maybeQuery =
            case query of
                NoQuery ->
                    Nothing

                Query _ _ text ->
                    Just text

        hash =
            uniqueId entry

        ariaSelected =
            if multiSelectable then
                Just (stringFromBool selected)

            else if selected then
                Just "true"

            else
                Nothing
    in
    viewOption
        { id = printEntryId id hash
        , role = "option"
        , ariaSelected = ariaSelected
        , onMouseenter = lift (EntryMouseEntered hash)
        , onMouseleave = lift EntryMouseLeft
        , onClick = lift (EntryClicked entry)
        }
        { selected = selected
        , focused = focused
        , hovered = hovered
        , maybeQuery = maybeQuery
        }
        entry


listKeyPress : Bool -> String -> KeyInfo -> Decoder (Msg a)
listKeyPress fromOutside id { code, altDown, controlDown, metaDown, shiftDown } =
    let
        noModifierDown =
            not (altDown || controlDown || metaDown || shiftDown)

        onlyShiftDown =
            not altDown && not controlDown && not metaDown && shiftDown

        onlyControlDown =
            not altDown && controlDown && not metaDown && not shiftDown

        notHandlingThatKey =
            Decode.fail "not handling that key combination"
    in
    case code of
        "ArrowUp" ->
            if noModifierDown then
                Decode.succeed (ListArrowUpDown id)

            else if onlyShiftDown then
                Decode.succeed (ListShiftArrowUpDown id)

            else
                notHandlingThatKey

        "ArrowDown" ->
            if noModifierDown then
                Decode.succeed (ListArrowDownDown id)

            else if onlyShiftDown then
                Decode.succeed (ListShiftArrowDownDown id)

            else
                notHandlingThatKey

        "Enter" ->
            if noModifierDown then
                Decode.succeed (ListEnterDown id)

            else
                notHandlingThatKey

        " " ->
            if not fromOutside then
                if onlyShiftDown then
                    Decode.succeed (ListShiftSpaceDown id)

                else if noModifierDown then
                    Decode.succeed (ListSpaceDown id)

                else
                    notHandlingThatKey

            else
                notHandlingThatKey

        "Home" ->
            if not altDown && controlDown && not metaDown && shiftDown then
                Decode.succeed (ListControlShiftHomeDown id)

            else if noModifierDown then
                Decode.succeed (ListHomeDown id)

            else
                notHandlingThatKey

        "End" ->
            if not altDown && controlDown && not metaDown && shiftDown then
                Decode.succeed (ListControlShiftEndDown id)

            else if noModifierDown then
                Decode.succeed (ListEndDown id)

            else
                notHandlingThatKey

        "a" ->
            if not fromOutside then
                if onlyControlDown then
                    Decode.succeed ListControlADown

                else if noModifierDown && (String.length code == 1) then
                    Decode.succeed (ListKeyDown id code)

                else
                    notHandlingThatKey

            else
                notHandlingThatKey

        _ ->
            if not fromOutside then
                if noModifierDown && (String.length code == 1) then
                    Decode.succeed (ListKeyDown id code)

                else
                    notHandlingThatKey

            else
                notHandlingThatKey


{-| **Available view customizations**

This is the second argument to `viewConfig`. You can customize the styling with
the following fields:

  - **ul**: A list of html attributes applied to the outer listbox.

  - **liOption**: A function returning `HtmlDetails` for each option in your
    entries list. It gets the actual option value `a` and flags telling you if
    this option is currently `selected`, `focused` or `hovered`. If the user
    typed in a query, you get this via the `maybeQuery` field.

  - **liDivider**: This lets you style the divider list entries. It gets the
    actual `divider` entry and returns `HtmlDetails`.

  - **empty**: What should be rendered when the listbox is empty?

  - **focusable**: Should the listbox be focusable?

The DOM structure of a listbox will be something like this:

    listbox =
        Html.ul
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

Provided you have specified some CSS classes, a view configuration could look
like this:

    views : Views String Never
    views =
        { ul = [ Html.Attributes.class "listbox__container" ]
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
        , empty = Html.text ""
        , focusable = True
        }

-}
type Views a node msg
    = Views
        { listbox : ListboxAttrs msg -> { options : List node } -> node
        , option :
            OptionAttrs msg
            ->
                { selected : Bool
                , focused : Bool
                , hovered : Bool
                , maybeQuery : Maybe String
                }
            -> a
            -> node
        }


type alias ListboxAttrs msg =
    { id : String
    , role : String
    , ariaMultiselectable : String
    , ariaLabelledby : Maybe String
    , ariaActivedescendant : Maybe String
    , tabindex : Maybe Int
    , preventDefaultOnKeydown : Decoder ( msg, Bool )
    , onMousedown : msg
    , onMouseup : msg
    , onFocus : msg
    , onBlur : msg
    }


type alias OptionAttrs msg =
    { id : String
    , role : String
    , ariaSelected : Maybe String
    , onMouseenter : msg
    , onMouseleave : msg
    , onClick : msg
    }


{-| TODO
-}
custom :
    { listbox : ListboxAttrs msg -> { options : List node } -> node
    , option :
        OptionAttrs msg
        ->
            { selected : Bool
            , focused : Bool
            , hovered : Bool
            , maybeQuery : Maybe String
            }
        -> a
        -> node
    }
    -> Views a node msg
custom =
    Views


{-| TODO
-}
html :
    { ul : List (Attribute msg)
    , li :
        { selected : Bool
        , focused : Bool
        , hovered : Bool
        , maybeQuery : Maybe String
        }
        -> a
        ->
            { attributes : List (Attribute msg)
            , children : List (Html msg)
            }
    }
    -> Views a (Html msg) msg
html config =
    Views
        { listbox =
            \attrs { options } ->
                let
                    addAriaLabelledBy htmlAttrs =
                        case attrs.ariaLabelledby of
                            Nothing ->
                                htmlAttrs

                            Just ariaLabelledby ->
                                Attributes.attribute "aria-labelledby" ariaLabelledby :: htmlAttrs

                    addAriaActivedescendant htmlAttrs =
                        case attrs.ariaActivedescendant of
                            Nothing ->
                                htmlAttrs

                            Just ariaActivedescendant ->
                                Attributes.attribute "aria-activedescendant" ariaActivedescendant
                                    :: htmlAttrs

                    addTabindex htmlAttrs =
                        case attrs.tabindex of
                            Nothing ->
                                htmlAttrs

                            Just tabindex ->
                                Attributes.tabindex tabindex :: htmlAttrs
                in
                Html.ul
                    (([ Attributes.id attrs.id
                      , Attributes.attribute "role" attrs.role
                      , Attributes.attribute "aria-multiselectable" attrs.ariaMultiselectable
                      , Events.preventDefaultOn "keydown" attrs.preventDefaultOnKeydown
                      , Events.onMouseDown attrs.onMousedown
                      , Events.onMouseUp attrs.onMouseup
                      , Events.onFocus attrs.onFocus
                      , Events.onBlur attrs.onFocus
                      ]
                        |> addAriaLabelledBy
                        |> addAriaActivedescendant
                        |> addTabindex
                     )
                        ++ config.ul
                    )
                    options
        , option =
            \attrs state a ->
                let
                    htmlDetails =
                        config.li state a

                    addAriaSelected htmlAttrs =
                        case attrs.ariaSelected of
                            Nothing ->
                                htmlAttrs

                            Just ariaSelected ->
                                Attributes.attribute "aria-selected" ariaSelected :: htmlAttrs
                in
                Html.li
                    (([ Attributes.id attrs.id
                      , Attributes.attribute "role" attrs.role
                      , Events.onMouseEnter attrs.onMouseenter
                      , Events.onMouseLeave attrs.onMouseleave
                      , Events.onClick attrs.onClick
                      ]
                        |> addAriaSelected
                     )
                        ++ htmlDetails.attributes
                    )
                    htmlDetails.children
        }


{-| This adds all the keydown event listener needed for the listbox on any DOM
node. For example, this could be an input field which keeps focused while the
listbox is displayed in a dropdown. You usually want to set `focusable = False`
inside the `ViewConfig` when using this event listener.

You must provide your own event decoder, which is tried **before** the
listbox's event decoder. This lets you prevent the listbox reacting on key
combinations. If you do not need to handle keydown events, just insert a failing decoder:

    view =
        Html.input
            [ preventDefaultOnKeyDown
                { id = "fruits-listbox"
                , labelledBy = "fruits"
                , lift = ListboxMsg
                }
                (Decode.fail "not handling this event here")
            ]
            []

In this example, pressing keys like `ArrowUp` and `ArrowDown` will adjust the
listbox's focus although the listbox itself is not focused.

-}
preventDefaultOnKeyDown : Instance a msg -> Decoder ( msg, Bool ) -> Decoder ( msg, Bool )
preventDefaultOnKeyDown instance decoder =
    Decode.oneOf
        [ decoder
        , Decode.andThen
            (listKeyPress True instance.id
                >> Decode.map (\msg -> ( instance.lift msg, True ))
            )
            KeyInfo.decoder
        ]


{-| Returns the HTML id of the currently focused entry.
-}
focusedEntryId :
    ViewConfig a
    -> Instance a msg
    -> List a
    -> Listbox
    -> Maybe String
focusedEntryId (ViewConfig config) instance entries (Listbox listbox) =
    case listbox.focus of
        NoFocus ->
            Nothing

        Focus current ->
            find config.uniqueId entries current
                |> Maybe.map (printEntryId instance.id << config.uniqueId)

        Pending { current } ->
            find config.uniqueId entries current
                |> Maybe.map (printEntryId instance.id << config.uniqueId)



---- UNIQUE SELECTION


{-| Use this instead of `view` if the user can only select **at
most one** entry in the listbox. The only difference between the type signature
of this function and the one of `view` is that the last argument is a `Maybe a`
instead of a `List a`.
-}
viewUnique :
    Views a node msg
    -> ViewConfig a
    -> Instance a msg
    -> List a
    -> Listbox
    -> Maybe a
    -> node
viewUnique views config instance entries listbox selection =
    viewHelp False views config instance entries listbox (maybeToList selection)


{-| Use this function instead of `update` if the user can only
select **at most one** entry in the listbox. The only difference between the
type signature of this function and the one of `update` is that the last
argument is a `Maybe a` instead of a `List a`.
-}
updateUnique :
    UpdateConfig a
    -> List a
    -> Msg a
    -> Listbox
    -> Maybe a
    -> ( Listbox, Cmd (Msg a), Maybe a )
updateUnique config allEntries msg listbox selection =
    let
        ( newListbox, cmd, newSelection ) =
            update config allEntries msg listbox <|
                maybeToList selection
    in
    ( newListbox, cmd, listToMaybe newSelection )


{-| Sets the keyboard focus to the provided options.

**Note**: This will not adjust the scroll position of the listbox, so you might
want to apply `scrollToFocus` afterwards.

-}
focusEntryUnique : UpdateConfig a -> a -> Listbox -> Maybe a -> ( Listbox, Maybe a )
focusEntryUnique config newEntry listbox selection =
    withUnique selection (focusEntry config newEntry listbox)


{-| Sets the keyboard focus to the next option. If `jumpAtEnds` is true and the
focus is already on the last option, the first option is selected.

**Note**: This will not adjust the scroll position of the listbox, so you might
want to apply `scrollToFocus` afterwards.

-}
focusNextOrFirstEntryUnique :
    UpdateConfig a
    -> List a
    -> Listbox
    -> Maybe a
    -> ( Listbox, Maybe a )
focusNextOrFirstEntryUnique config allEntries listbox selection =
    withUnique selection (focusNextOrFirstEntry config allEntries listbox)


{-| Sets the keyboard focus to the previous option. If `jumpAtEnds` is true and the
focus is already on the first option, the first option is selected.

**Note**: This will not adjust the scroll position of the listbox, so you might
want to apply `scrollToFocus` afterwards.

-}
focusPreviousOrFirstEntryUnique :
    UpdateConfig a
    -> List a
    -> Listbox
    -> Maybe a
    -> ( Listbox, Maybe a )
focusPreviousOrFirstEntryUnique config allEntries listbox selection =
    withUnique selection (focusPreviousOrFirstEntry config allEntries listbox)


withUnique : Maybe a -> (List a -> ( Listbox, List a )) -> ( Listbox, Maybe a )
withUnique selection func =
    let
        ( listbox, list ) =
            func (maybeToList selection)
    in
    ( listbox, listToMaybe list )


maybeToList : Maybe a -> List a
maybeToList maybeA =
    case maybeA of
        Nothing ->
            []

        Just a ->
            [ a ]


listToMaybe : List a -> Maybe a
listToMaybe listA =
    case listA of
        [] ->
            Nothing

        a :: _ ->
            Just a



---- UPDATE


{-| The listbox's message type.
-}
type Msg a
    = NoOp
    | EntryDomDataReceived a (Result Dom.Error EntryDomData)
    | ViewportOfListReceived Direction a (Result Dom.Error Dom.Viewport)
      -- LIST
    | ListMouseDown
    | ListMouseUp
    | ListFocused String
    | ListBlured
    | ListArrowUpDown String
    | ListShiftArrowUpDown String
    | ListArrowDownDown String
    | ListShiftArrowDownDown String
    | ListEnterDown String
    | ListSpaceDown String
    | ListShiftSpaceDown String
    | ListHomeDown String
    | ListControlShiftHomeDown String
    | ListEndDown String
    | ListControlShiftEndDown String
    | ListControlADown
      -- QUERY
    | ListKeyDown String String
    | CurrentTimeReceived String String Time.Posix
    | Tick Time.Posix
      -- ENTRY
    | EntryMouseEntered String
    | EntryMouseLeft
    | EntryClicked a


type Direction
    = Up
    | Down


type Effect a
    = CmdNone
    | TimeNow (Posix -> Msg a)
    | DomFocus String
      -- SCROLLING
    | GetViewportOfList (Result Dom.Error Dom.Viewport -> Msg a) String
    | ScrollListToTop String
    | ScrollListToBottom String
    | ScrollToOption (Behaviour a) String String (Maybe String)
    | GetViewportOf (Result Dom.Error EntryDomData -> Msg a) (Behaviour a) String String String
    | SetViewportOf String Float Float


{-| Use this function to update the listbox state. You have to provide the same
entries and selection as given to your view function.

For example:

    update msg model =
        case msg of
            ListboxMsg listboxMsg ->
                let
                    ( newListbox, listboxCmd, newSelection ) =
                        Listbox.update updateConfig
                            entries
                            listboxMsg
                            model.listbox
                            model.selection
                in
                ( { model
                    | listbox = newListbox
                    , selection = newSelection
                  }
                , Cmd.map ListboxMsg listboxCmd
                )

In a more sophisticated example, the entries could be dynamic, as well. (For
example, loaded via an HTTP request.)

-}
update :
    UpdateConfig a
    -> List a
    -> Msg a
    -> Listbox
    -> List a
    -> ( Listbox, Cmd (Msg a), List a )
update config entries msg listbox selection =
    let
        ( newListbox, effect, newSelection ) =
            updateHelp config entries msg listbox selection
    in
    ( newListbox, perform effect, newSelection )


updateHelp :
    UpdateConfig a
    -> List a
    -> Msg a
    -> Listbox
    -> List a
    -> ( Listbox, Effect a, List a )
updateHelp ((UpdateConfig { uniqueId, behaviour }) as config) allEntries msg (Listbox listbox) selection =
    let
        unchanged =
            ( Listbox listbox
            , CmdNone
            , selection
            )

        fromModel (Listbox newListbox) =
            ( Listbox newListbox
            , CmdNone
            , selection
            )

        withEffect effect ( Listbox newListbox, _, newSelection ) =
            ( Listbox newListbox, effect, newSelection )

        withSelection newSelection ( Listbox newListbox, effect, _ ) =
            ( Listbox newListbox, effect, newSelection )

        -- SELECTION
        select a listA ( Listbox newListbox, effect, newSelection ) =
            Listbox { newListbox | maybeLastSelectedEntry = Just (uniqueId a) }
                |> fromModel
                |> withSelection (List.uniqueBy uniqueId (a :: listA ++ newSelection))

        unselect a ( Listbox newListbox, effect, newSelection ) =
            Listbox newListbox
                |> fromModel
                |> withSelection (List.filter (\b -> a /= b) newSelection)

        toggle a ( Listbox newListbox, effect, newSelection ) =
            if List.member a newSelection then
                Listbox newListbox
                    |> fromModel
                    |> withSelection (List.filter (\b -> a /= b) newSelection)

            else
                Listbox { newListbox | maybeLastSelectedEntry = Just (uniqueId a) }
                    |> fromModel
                    |> withSelection (List.uniqueBy uniqueId (a :: newSelection))

        -- FOCUS
        initFocus id =
            let
                maybeA =
                    listbox.focus
                        |> currentFocus
                        |> or listbox.maybeLastSelectedEntry
                        |> Maybe.andThen (find uniqueId allEntries)
                        |> or (List.head selection)
                        |> Maybe.andThen (uniqueId >> find uniqueId allEntries)
                        |> or (List.head allEntries)
            in
            case maybeA of
                Nothing ->
                    fromModel (Listbox { listbox | query = NoQuery })

                Just a ->
                    let
                        hash =
                            uniqueId a

                        newListbox =
                            Listbox
                                { listbox
                                    | query = NoQuery
                                    , focus = Focus hash
                                }
                    in
                    if behaviour.selectionFollowsFocus then
                        newListbox
                            |> fromModel
                            |> withEffect (ScrollToOption behaviour id hash Nothing)
                            |> select a []

                    else
                        newListbox
                            |> fromModel
                            |> withEffect (ScrollToOption behaviour id hash Nothing)

        scheduleFocusPrevious id shiftDown current =
            case findPrevious uniqueId allEntries current of
                Just (Last a) ->
                    if behaviour.jumpAtEnds then
                        { listbox
                            | query = NoQuery
                            , focus =
                                Pending
                                    { id = id
                                    , current = current
                                    , pending = uniqueId a
                                    , shiftDown = shiftDown
                                    }
                        }
                            |> Listbox
                            |> fromModel
                            |> withEffect
                                (GetViewportOfList (ViewportOfListReceived Up a) id)

                    else if behaviour.selectionFollowsFocus then
                        case find uniqueId allEntries current of
                            Nothing ->
                                fromModel (Listbox { listbox | query = NoQuery })

                            Just currentA ->
                                if shiftDown then
                                    fromModel (Listbox { listbox | query = NoQuery })
                                        |> toggle currentA

                                else
                                    fromModel (Listbox { listbox | query = NoQuery })
                                        |> withSelection [ currentA ]

                    else
                        fromModel (Listbox { listbox | query = NoQuery })

                Just (Previous a) ->
                    let
                        hash =
                            uniqueId a
                    in
                    { listbox
                        | query = NoQuery
                        , focus =
                            Pending
                                { id = id
                                , current = current
                                , pending = hash
                                , shiftDown = shiftDown
                                }
                    }
                        |> Listbox
                        |> fromModel
                        |> withEffect (GetViewportOf (EntryDomDataReceived a) behaviour id hash current)

                Nothing ->
                    initFocus id

        scheduleFocusNext id shiftDown current =
            case findNext uniqueId allEntries current of
                Just (First a) ->
                    if behaviour.jumpAtEnds then
                        { listbox
                            | query = NoQuery
                            , focus =
                                Pending
                                    { id = id
                                    , current = current
                                    , pending = uniqueId a
                                    , shiftDown = shiftDown
                                    }
                        }
                            |> Listbox
                            |> fromModel
                            |> withEffect
                                (GetViewportOfList (ViewportOfListReceived Down a) id)

                    else if behaviour.selectionFollowsFocus then
                        case find uniqueId allEntries current of
                            Nothing ->
                                fromModel (Listbox { listbox | query = NoQuery })

                            Just currentA ->
                                if shiftDown then
                                    fromModel (Listbox { listbox | query = NoQuery })
                                        |> toggle currentA

                                else
                                    fromModel (Listbox { listbox | query = NoQuery })
                                        |> withSelection [ currentA ]

                    else
                        fromModel (Listbox { listbox | query = NoQuery })

                Just (Next a) ->
                    let
                        hash =
                            uniqueId a
                    in
                    Listbox
                        { listbox
                            | query = NoQuery
                            , focus =
                                Pending
                                    { id = id
                                    , current = current
                                    , pending = hash
                                    , shiftDown = shiftDown
                                    }
                        }
                        |> fromModel
                        |> withEffect (GetViewportOf (EntryDomDataReceived a) behaviour id hash current)

                Nothing ->
                    initFocus id
    in
    case msg of
        NoOp ->
            unchanged

        EntryDomDataReceived a result ->
            case result of
                Err id ->
                    unchanged

                Ok entryDomData ->
                    case listbox.focus of
                        NoFocus ->
                            unchanged

                        Focus _ ->
                            unchanged

                        Pending { id, pending, shiftDown } ->
                            let
                                newListbox =
                                    { listbox | focus = Focus pending }

                                ( x, y ) =
                                    newPosition behaviour entryDomData
                            in
                            if behaviour.selectionFollowsFocus && not shiftDown then
                                newListbox
                                    |> Listbox
                                    |> fromModel
                                    |> withSelection [ a ]
                                    |> withEffect (SetViewportOf id x y)

                            else if shiftDown then
                                newListbox
                                    |> Listbox
                                    |> fromModel
                                    |> toggle a
                                    |> withEffect (SetViewportOf id x y)

                            else
                                fromModel (Listbox newListbox)
                                    |> withEffect (SetViewportOf id x y)

        ViewportOfListReceived direction a result ->
            case result of
                Err id ->
                    unchanged

                Ok viewport ->
                    case listbox.focus of
                        NoFocus ->
                            unchanged

                        Focus _ ->
                            unchanged

                        Pending { id, pending, shiftDown } ->
                            let
                                newListbox =
                                    { listbox | focus = Focus pending }

                                effect =
                                    case direction of
                                        Up ->
                                            SetViewportOf id
                                                viewport.viewport.x
                                                viewport.scene.height

                                        Down ->
                                            SetViewportOf id
                                                viewport.viewport.x
                                                0
                            in
                            if behaviour.selectionFollowsFocus && not shiftDown then
                                newListbox
                                    |> Listbox
                                    |> fromModel
                                    |> withSelection [ a ]
                                    |> withEffect effect

                            else if shiftDown then
                                newListbox
                                    |> Listbox
                                    |> fromModel
                                    |> toggle a
                                    |> withEffect effect

                            else
                                newListbox
                                    |> Listbox
                                    |> fromModel
                                    |> withEffect effect

        -- LIST
        ListMouseDown ->
            fromModel (Listbox { listbox | preventScroll = True })

        ListMouseUp ->
            fromModel (Listbox { listbox | preventScroll = False })

        ListFocused id ->
            if listbox.preventScroll then
                unchanged

            else
                initFocus id

        ListBlured ->
            fromModel
                (Listbox
                    { listbox
                        | query = NoQuery
                        , preventScroll = False
                    }
                )

        ListArrowUpDown id ->
            case listbox.focus of
                NoFocus ->
                    initFocus id

                Focus hash ->
                    scheduleFocusPrevious id False hash

                Pending _ ->
                    unchanged

        ListShiftArrowUpDown id ->
            case listbox.focus of
                NoFocus ->
                    initFocus id

                Focus hash ->
                    scheduleFocusPrevious id True hash

                Pending _ ->
                    unchanged

        ListArrowDownDown id ->
            case listbox.focus of
                NoFocus ->
                    initFocus id

                Focus hash ->
                    scheduleFocusNext id False hash

                Pending _ ->
                    unchanged

        ListShiftArrowDownDown id ->
            case listbox.focus of
                NoFocus ->
                    initFocus id

                Focus hash ->
                    scheduleFocusNext id True hash

                Pending _ ->
                    unchanged

        ListEnterDown id ->
            case focusedEntry config allEntries (Listbox listbox) of
                Nothing ->
                    unchanged

                Just a ->
                    unchanged
                        |> toggle a

        ListSpaceDown id ->
            case focusedEntry config allEntries (Listbox listbox) of
                Nothing ->
                    unchanged

                Just a ->
                    unchanged
                        |> toggle a

        ListShiftSpaceDown id ->
            let
                selected =
                    Maybe.map2 (range uniqueId allEntries)
                        (currentFocus listbox.focus)
                        listbox.maybeLastSelectedEntry
                        |> Maybe.withDefault []
            in
            case selected of
                [] ->
                    unchanged

                a :: listA ->
                    unchanged
                        |> select a listA

        ListHomeDown id ->
            case List.head allEntries of
                Nothing ->
                    unchanged

                Just a ->
                    { listbox
                        | query = NoQuery
                        , focus = Focus (uniqueId a)
                    }
                        |> Listbox
                        |> fromModel
                        |> withEffect (ScrollListToTop id)

        ListControlShiftHomeDown id ->
            case Maybe.map uniqueId (List.head allEntries) of
                Nothing ->
                    unchanged

                Just hash ->
                    let
                        selected =
                            listbox.focus
                                |> currentFocus
                                |> Maybe.map (range uniqueId allEntries hash)
                                |> Maybe.withDefault []
                    in
                    case selected of
                        [] ->
                            unchanged

                        a :: listA ->
                            { listbox
                                | focus = Focus hash
                                , hover =
                                    if behaviour.separateFocus then
                                        listbox.hover

                                    else
                                        Just hash
                            }
                                |> Listbox
                                |> fromModel
                                |> select a listA
                                |> withEffect (ScrollListToTop id)

        ListEndDown id ->
            case lastEntry allEntries of
                Nothing ->
                    unchanged

                Just a ->
                    { listbox
                        | query = NoQuery
                        , focus = Focus (uniqueId a)
                    }
                        |> Listbox
                        |> fromModel
                        |> withEffect (ScrollListToBottom id)

        ListControlShiftEndDown id ->
            case Maybe.map uniqueId (lastEntry allEntries) of
                Nothing ->
                    unchanged

                Just hash ->
                    let
                        selected =
                            listbox.focus
                                |> currentFocus
                                |> Maybe.map (range uniqueId allEntries hash)
                                |> Maybe.withDefault []
                    in
                    case selected of
                        [] ->
                            unchanged

                        a :: listA ->
                            { listbox
                                | focus = Focus hash
                                , hover =
                                    if behaviour.separateFocus then
                                        listbox.hover

                                    else
                                        Just hash
                            }
                                |> Listbox
                                |> fromModel
                                |> select a listA
                                |> withEffect (ScrollListToBottom id)

        ListControlADown ->
            let
                allEntriesSet =
                    allEntries
                        |> List.map uniqueId
                        |> Set.fromList

                selectionSet =
                    selection
                        |> List.map uniqueId
                        |> Set.fromList
            in
            if Set.isEmpty (Set.diff allEntriesSet selectionSet) then
                unchanged
                    |> withSelection []

            else
                unchanged
                    |> withSelection allEntries

        -- QUERY
        ListKeyDown id key ->
            case behaviour.typeAhead of
                NoTypeAhead ->
                    unchanged

                TypeAhead _ _ ->
                    unchanged
                        |> withEffect (TimeNow (CurrentTimeReceived id key))

        CurrentTimeReceived id key currentTime ->
            case behaviour.typeAhead of
                NoTypeAhead ->
                    unchanged

                TypeAhead timeout matchesQuery ->
                    let
                        ( newQuery, queryText ) =
                            case listbox.query of
                                NoQuery ->
                                    ( Query timeout currentTime key, key )

                                Query _ _ query ->
                                    ( Query timeout currentTime (query ++ key), query ++ key )

                        maybeHash =
                            Maybe.andThen
                                (findWith matchesQuery uniqueId queryText allEntries)
                                (currentFocus listbox.focus)
                    in
                    case maybeHash of
                        Nothing ->
                            unchanged

                        Just hash ->
                            { listbox
                                | query = newQuery
                                , focus = Focus hash
                                , hover =
                                    if behaviour.separateFocus then
                                        listbox.hover

                                    else
                                        Just hash
                            }
                                |> Listbox
                                |> fromModel
                                |> withEffect (ScrollToOption behaviour id hash Nothing)

        Tick currentTime ->
            case listbox.query of
                NoQuery ->
                    unchanged

                Query timeout time _ ->
                    if
                        (Time.posixToMillis currentTime - Time.posixToMillis time)
                            > timeout
                    then
                        fromModel (Listbox { listbox | query = NoQuery })

                    else
                        unchanged

        -- ENTRY
        EntryMouseEntered newFocus ->
            fromModel
                (Listbox
                    { listbox
                        | focus =
                            if behaviour.separateFocus then
                                listbox.focus

                            else
                                Focus newFocus
                        , hover = Just newFocus
                    }
                )

        EntryMouseLeft ->
            fromModel
                (Listbox
                    { listbox
                        | hover =
                            if behaviour.separateFocus then
                                Nothing

                            else
                                listbox.hover
                    }
                )

        EntryClicked a ->
            let
                hash =
                    uniqueId a
            in
            if behaviour.selectionFollowsFocus then
                { listbox
                    | query = NoQuery
                    , focus = Focus hash
                    , hover = Just hash
                }
                    |> Listbox
                    |> fromModel
                    |> select a selection

            else
                { listbox
                    | query = NoQuery
                    , focus = Focus hash
                    , hover = Just hash
                }
                    |> Listbox
                    |> fromModel
                    |> toggle a


type alias InitialEntryDomData =
    { viewportList : Dom.Viewport
    , elementList : Dom.Element
    , elementLi : Dom.Element
    }


focusPendingKeyboardFocus : Listbox -> Listbox
focusPendingKeyboardFocus (Listbox listbox) =
    case listbox.focus of
        NoFocus ->
            Listbox listbox

        Focus _ ->
            Listbox listbox

        Pending { pending } ->
            Listbox { listbox | focus = Focus pending }


newPosition : Behaviour a -> EntryDomData -> ( Float, Float )
newPosition behaviour entryDomData =
    let
        ---- SCROLLING
        viewport =
            entryDomData.viewportList.viewport

        list =
            entryDomData.elementList

        li =
            entryDomData.elementLi

        previousLi =
            entryDomData.elementPreviousLi

        -- MEASUREMENTS
        liY =
            li.element.y - list.element.y + viewport.y

        liHeight =
            li.element.height

        previousLiY =
            previousLi.element.y - list.element.y + viewport.y

        previousLiHeight =
            previousLi.element.height

        -- CONDITIONS
        previousEntryHidden =
            (previousLiY + previousLiHeight < viewport.y)
                || (previousLiY > viewport.y + viewport.height)

        newEntryTooLow =
            liY + liHeight + behaviour.minimalGap > viewport.y + viewport.height

        newEntryTooHigh =
            liY - behaviour.minimalGap < viewport.y
    in
    if previousEntryHidden then
        ( viewport.x
        , liY + liHeight / 2 - viewport.height / 2
        )

    else if newEntryTooLow then
        ( viewport.x
        , liY + liHeight - viewport.height + behaviour.initialGap
        )

    else if newEntryTooHigh then
        ( viewport.x
        , liY - behaviour.initialGap
        )

    else
        ( viewport.x
        , viewport.y
        )


andDo : effect -> ( a, b ) -> ( a, effect, b )
andDo effect ( a, b ) =
    ( a, effect, b )


or : Maybe a -> Maybe a -> Maybe a
or fallback default =
    case default of
        Nothing ->
            fallback

        Just _ ->
            default


type alias EntryDomData =
    { viewportList : Dom.Viewport
    , elementList : Dom.Element
    , elementLi : Dom.Element
    , elementPreviousLi : Dom.Element
    }


perform : Effect a -> Cmd (Msg a)
perform effect =
    case effect of
        CmdNone ->
            Cmd.none

        TimeNow toMsg ->
            Task.perform toMsg Time.now

        DomFocus id ->
            Task.attempt (\_ -> NoOp) <|
                Dom.focus id

        -- SCROLLING
        GetViewportOfList toMsg id ->
            Dom.getViewportOf (printListId id)
                |> Task.attempt toMsg

        ScrollListToTop id ->
            Dom.getViewportOf (printListId id)
                |> Task.andThen
                    (\list ->
                        Dom.setViewportOf (printListId id)
                            list.viewport.x
                            0
                    )
                |> Task.attempt (\_ -> NoOp)

        ScrollListToBottom id ->
            Dom.getViewportOf (printListId id)
                |> Task.andThen
                    (\list ->
                        Dom.setViewportOf (printListId id)
                            list.viewport.x
                            list.scene.height
                    )
                |> Task.attempt (\_ -> NoOp)

        GetViewportOf toMsg behaviour id hash previousHash ->
            Task.map4 EntryDomData
                (Dom.getViewportOf (printListId id))
                (Dom.getElement (printListId id))
                (Dom.getElement (printEntryId id hash))
                (Dom.getElement (printEntryId id previousHash))
                |> Task.attempt toMsg

        SetViewportOf id x y ->
            Dom.setViewportOf (printListId id) x y
                |> Task.attempt (\_ -> NoOp)

        ScrollToOption behaviour id hash maybePreviousHash ->
            case maybePreviousHash of
                Nothing ->
                    Task.map3 InitialEntryDomData
                        (Dom.getViewportOf (printListId id))
                        (Dom.getElement (printListId id))
                        (Dom.getElement (printEntryId id hash))
                        |> Task.andThen
                            (\{ viewportList, elementList, elementLi } ->
                                let
                                    { viewport } =
                                        viewportList

                                    liY =
                                        elementLi.element.y - elementList.element.y + viewport.y

                                    liHeight =
                                        elementLi.element.height

                                    entryHidden =
                                        (liY + liHeight - behaviour.minimalGap < viewport.y)
                                            || (liY + behaviour.minimalGap > viewport.y + viewport.height)
                                in
                                if entryHidden then
                                    Dom.setViewportOf
                                        (printListId id)
                                        viewport.x
                                        (liY + liHeight / 2 - viewport.height / 2)

                                else
                                    Task.succeed ()
                            )
                        |> Task.attempt (\_ -> NoOp)

                Just previousHash ->
                    Task.map4 EntryDomData
                        (Dom.getViewportOf (printListId id))
                        (Dom.getElement (printListId id))
                        (Dom.getElement (printEntryId id hash))
                        (Dom.getElement (printEntryId id previousHash))
                        |> Task.andThen
                            (\entryDomData ->
                                let
                                    viewport =
                                        entryDomData.viewportList.viewport

                                    list =
                                        entryDomData.elementList

                                    li =
                                        entryDomData.elementLi

                                    previousLi =
                                        entryDomData.elementPreviousLi

                                    -- MEASUREMENTS
                                    liY =
                                        li.element.y - list.element.y + viewport.y

                                    liHeight =
                                        li.element.height

                                    previousLiY =
                                        previousLi.element.y - list.element.y + viewport.y

                                    previousLiHeight =
                                        previousLi.element.height

                                    -- CONDITIONS
                                    previousEntryHidden =
                                        (previousLiY + previousLiHeight < viewport.y)
                                            || (previousLiY > viewport.y + viewport.height)

                                    newEntryTooLow =
                                        liY + liHeight + behaviour.minimalGap > viewport.y + viewport.height

                                    newEntryTooHigh =
                                        liY - behaviour.minimalGap < viewport.y

                                    -- EFFECT
                                    centerNewEntry =
                                        domSetViewportOf viewport.x <|
                                            (liY + liHeight / 2 - viewport.height / 2)

                                    scrollDownToNewEntry =
                                        domSetViewportOf viewport.x <|
                                            (liY + liHeight - viewport.height + behaviour.initialGap)

                                    scrollUpToNewEntry =
                                        domSetViewportOf viewport.x <|
                                            (liY - behaviour.initialGap)

                                    domSetViewportOf x y =
                                        Dom.setViewportOf (printListId id) x y
                                in
                                if previousEntryHidden then
                                    centerNewEntry

                                else if newEntryTooLow then
                                    scrollDownToNewEntry

                                else if newEntryTooHigh then
                                    scrollUpToNewEntry

                                else
                                    Task.succeed ()
                            )
                        |> Task.attempt (\_ -> NoOp)



---- SUBSCRIPTIONS


{-| Do not forget to add this to your subscriptions:

    subscriptions model =
        Sub.map ListboxMsg
            (Listbox.subscriptions model.listbox)

-}
subscriptions : Listbox -> Sub (Msg a)
subscriptions (Listbox listbox) =
    case listbox.query of
        NoQuery ->
            Sub.none

        Query timeout _ _ ->
            Time.every (toFloat (timeout // 3)) Tick



-- IDS


printListId : String -> String
printListId id =
    id


printEntryId : String -> String -> String
printEntryId id entryId =
    id ++ "__element--" ++ entryId



-- FIND


indexOf : (a -> String) -> List a -> String -> Maybe Int
indexOf uniqueId entries selectedId =
    findHelp 0 uniqueId entries selectedId
        |> Maybe.map Tuple.first


find : (a -> String) -> List a -> String -> Maybe a
find uniqueId entries selectedId =
    findHelp 0 uniqueId entries selectedId
        |> Maybe.map Tuple.second


findHelp :
    Int
    -> (a -> String)
    -> List a
    -> String
    -> Maybe ( Int, a )
findHelp index uniqueId entries selectedId =
    case entries of
        [] ->
            Nothing

        entry :: rest ->
            if uniqueId entry == selectedId then
                Just ( index, entry )

            else
                findHelp (index + 1) uniqueId rest selectedId


findWith :
    (String -> a -> Bool)
    -> (a -> String)
    -> String
    -> List a
    -> String
    -> Maybe String
findWith matchesQuery uniqueId query entries id =
    case entries of
        [] ->
            Nothing

        a :: rest ->
            if uniqueId a == id then
                if matchesQuery query a then
                    Just id

                else
                    proceedWith matchesQuery uniqueId id query rest

            else
                findWith matchesQuery uniqueId query rest id


proceedWith :
    (String -> a -> Bool)
    -> (a -> String)
    -> String
    -> String
    -> List a
    -> Maybe String
proceedWith matchesQuery uniqueId id query entries =
    case entries of
        [] ->
            Just id

        a :: rest ->
            if matchesQuery query a then
                Just (uniqueId a)

            else
                proceedWith matchesQuery uniqueId id query rest


lastEntry : List a -> Maybe a
lastEntry entries =
    List.head (List.reverse entries)



-- PREVIOUS


type Previous a
    = Previous a
    | Last a


findPrevious :
    (a -> String)
    -> List a
    -> String
    -> Maybe (Previous a)
findPrevious uniqueId entries currentId =
    case entries of
        [] ->
            Nothing

        first :: rest ->
            if uniqueId first == currentId then
                entries
                    |> lastEntry
                    |> Maybe.map Last

            else
                findPreviousHelp first uniqueId rest currentId


findPreviousHelp :
    a
    -> (a -> String)
    -> List a
    -> String
    -> Maybe (Previous a)
findPreviousHelp previous uniqueId entries currentId =
    case entries of
        [] ->
            Nothing

        first :: rest ->
            if uniqueId first == currentId then
                Just (Previous previous)

            else
                findPreviousHelp first uniqueId rest currentId



-- NEXT


type Next a
    = Next a
    | First a


findNext : (a -> String) -> List a -> String -> Maybe (Next a)
findNext uniqueId entries currentId =
    case entries of
        [] ->
            Nothing

        first :: rest ->
            if uniqueId first == currentId then
                List.head rest
                    |> Maybe.map Next

            else
                Just (findNextHelp first uniqueId rest currentId)


findNextHelp :
    a
    -> (a -> String)
    -> List a
    -> String
    -> Next a
findNextHelp first uniqueId entries currentId =
    case entries of
        [] ->
            First first

        a :: rest ->
            if uniqueId a == currentId then
                List.head rest
                    |> Maybe.map Next
                    |> Maybe.withDefault (First first)

            else
                findNextHelp first uniqueId rest currentId



---- RANGE


range : (a -> String) -> List a -> String -> String -> List a
range uniqueId entries end start =
    case entries of
        [] ->
            []

        a :: rest ->
            if uniqueId a == start then
                rangeHelp uniqueId [ a ] end rest

            else if uniqueId a == end then
                List.reverse (rangeHelp uniqueId [ a ] start rest)

            else
                range uniqueId rest start end


rangeHelp : (a -> String) -> List a -> String -> List a -> List a
rangeHelp uniqueId collected end entries =
    case entries of
        [] ->
            []

        a :: rest ->
            if uniqueId a == end then
                a :: collected

            else
                rangeHelp uniqueId (a :: collected) end rest
