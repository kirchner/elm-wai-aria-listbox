module Listbox exposing
    ( view, Instance, Label, labelledBy, label, noLabel
    , Entry, option, divider
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
    , customView, customViewUnique
    , DomFunctions
    , CustomViewConfig, customViewConfig, CustomViews
    , customFocusedEntryId
    , customPreventDefaultOnKeyDown
    )

{-| Implementation of the [listbox
widget](https://www.w3.org/TR/wai-aria-practices-1.1/#Listbox):

> A listbox widget presents a list of options and allows a user to select one
> or more of them.

Take a look at the documentation of `Behaviour` for the default keyboard
interactions this widget offers.


# View

@docs view, Instance, Label, labelledBy, label, noLabel


# Entry

@docs Entry, option, divider


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

@docs customView, customViewUnique

@docs DomFunctions

@docs CustomViewConfig, customViewConfig, CustomViews

@docs customFocusedEntryId

@docs customPreventDefaultOnKeyDown

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
import Internal.Label as Internal
import Internal.Listbox as Internal
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
type alias Listbox =
    Internal.Listbox


{-| An initial listbox with no option focused.
-}
init : Listbox
init =
    Internal.init


{-| When updating or viewing a listbox you have to provide a list of entries.
These can be selectable options or non-selectable dividers. You can construct
these using `option` and `divider`.
-}
type alias Entry a divider =
    Internal.Entry a divider


{-| Create a selectable option.
-}
option : a -> Entry a divider
option =
    Internal.Option


{-| Create a non-selectable divider.
-}
divider : divider -> Entry a divider
divider =
    Internal.Divider



---- EXTERNAL STATE MANIPULATION


{-| A task to give the listbox focus. The first argument must match the
`Instance` used in the `view` function!
-}
focus : Instance a msg -> Task Dom.Error ()
focus { id } =
    Dom.focus (Internal.printListId id)


{-| Returns the option which currently has keyboard focus.
-}
focusedEntry :
    UpdateConfig a
    -> List (Entry a divider)
    -> Listbox
    -> Maybe a
focusedEntry (UpdateConfig config) =
    Internal.focusedEntry config


{-| Returns the option which currently has mouse focus.
-}
hoveredEntry :
    UpdateConfig a
    -> List (Entry a divider)
    -> Listbox
    -> Maybe a
hoveredEntry (UpdateConfig config) =
    Internal.hoveredEntry config


{-| Sets the keyboard focus to the provided options.

**Note**: This will not adjust the scroll position of the listbox, so you might
want to apply `scrollToFocus` afterwards.

-}
focusEntry :
    UpdateConfig a
    -> a
    -> Listbox
    -> List a
    -> ( Listbox, List a )
focusEntry (UpdateConfig config) newEntry listbox selection =
    Internal.focusEntry config newEntry listbox selection


{-| Sets the keyboard focus to the next option. If `jumpAtEnds` is true and the
focus is already on the last option, the first option is selected.

**Note**: This will not adjust the scroll position of the listbox, so you might
want to apply `scrollToFocus` afterwards.

-}
focusNextOrFirstEntry :
    UpdateConfig a
    -> List (Entry a divider)
    -> Listbox
    -> List a
    -> ( Listbox, List a )
focusNextOrFirstEntry (UpdateConfig config) allEntries listbox selection =
    Internal.focusNextOrFirstEntry config allEntries listbox selection


{-| Sets the keyboard focus to the previous option. If `jumpAtEnds` is true and the
focus is already on the first option, the first option is selected.

**Note**: This will not adjust the scroll position of the listbox, so you might
want to apply `scrollToFocus` afterwards.

-}
focusPreviousOrFirstEntry :
    UpdateConfig a
    -> List (Entry a divider)
    -> Listbox
    -> List a
    -> ( Listbox, List a )
focusPreviousOrFirstEntry (UpdateConfig config) allEntries listbox selection =
    Internal.focusPreviousOrFirstEntry config allEntries listbox selection


{-| A command adjusting the scroll position of the listbox such that the
current keyboard focus is visible.
-}
scrollToFocus : Behaviour a -> Instance a msg -> Listbox -> Cmd msg
scrollToFocus behaviour { id, lift } listbox =
    Cmd.map lift (perform (Internal.scrollToFocus behaviour id listbox))



---- VIEW CONFIG


{-| -}
type ViewConfig a divider
    = ViewConfig (Internal.ViewConfig a)


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
    -> ViewConfig a divider
viewConfig =
    ViewConfig


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
type alias Views a divider =
    { ul : HtmlAttributes
    , liOption :
        { selected : Bool
        , focused : Bool
        , hovered : Bool
        , maybeQuery : Maybe String
        }
        -> a
        -> HtmlDetails
    , liDivider : divider -> HtmlDetails
    , empty : Html Never
    , focusable : Bool
    , markActiveDescendant : Bool
    }


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
    = UpdateConfig (Internal.UpdateConfig a)


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
type alias TypeAhead a =
    Internal.TypeAhead a


{-| Use this inside `Behaviour` if you do not want to activate the type-ahead
functionality.
-}
noTypeAhead : TypeAhead a
noTypeAhead =
    Internal.NoTypeAhead


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
    Internal.TypeAhead timeout <|
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
    Internal.TypeAhead



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
view :
    Internal.Views a divider (Attribute Never) (Html Never)
    -> ViewConfig a divider
    -> Instance a msg
    -> List (Entry a divider)
    -> Listbox
    -> List a
    -> Html msg
view views (ViewConfig config) instance allEntries listbox selection =
    Internal.view True htmlFunctions views config instance allEntries listbox selection


htmlFunctions : DomFunctions (Attribute msg) (Attribute Never) (Html msg) (Html Never) msg
htmlFunctions =
    { ul = Html.ul
    , li = Html.li
    , on = Events.on
    , preventDefaultOn = Events.preventDefaultOn
    , property = Attributes.property
    , attribute = Attributes.attribute
    , attributeMap = \noOp -> Attributes.map (\_ -> noOp)
    , htmlMap = \noOp -> Html.map (\_ -> noOp)
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
preventDefaultOnKeyDown : Instance a msg -> Decoder ( msg, Bool ) -> Attribute msg
preventDefaultOnKeyDown =
    Internal.preventDefaultOnKeyDown Events.preventDefaultOn


{-| Returns the HTML id of the currently focused entry.
-}
focusedEntryId :
    ViewConfig a divider
    -> Instance a msg
    -> List (Entry a divider)
    -> Listbox
    -> Maybe String
focusedEntryId (ViewConfig config) =
    Internal.focusedEntryId config



---- UNIQUE SELECTION


{-| Use this instead of `view` if the user can only select **at
most one** entry in the listbox. The only difference between the type signature
of this function and the one of `view` is that the last argument is a `Maybe a`
instead of a `List a`.
-}
viewUnique :
    Internal.Views a divider (Attribute Never) (Html Never)
    -> ViewConfig a divider
    -> Instance a msg
    -> List (Entry a divider)
    -> Listbox
    -> Maybe a
    -> Html msg
viewUnique views (ViewConfig config) cfg entries listbox selection =
    Internal.view False htmlFunctions views config cfg entries listbox (maybeToList selection)


{-| Use this instead of `customView` if the user can only select **at
most one** entry in the listbox. The only difference between the type signature
of this function and the one of `customView` is that the last argument is
a `Maybe a` instead of a `List a`.
-}
customViewUnique :
    DomFunctions attribute attributeNever html htmlNever msg
    -> Internal.Views a divider attributeNever htmlNever
    -> CustomViewConfig a
    -> Instance a msg
    -> List (Entry a divider)
    -> Listbox
    -> Maybe a
    -> html
customViewUnique dom views (CustomViewConfig config) cfg entries listbox selection =
    Internal.view False dom views config cfg entries listbox (maybeToList selection)


{-| Use this function instead of `update` if the user can only
select **at most one** entry in the listbox. The only difference between the
type signature of this function and the one of `update` is that the last
argument is a `Maybe a` instead of a `List a`.
-}
updateUnique :
    UpdateConfig a
    -> List (Entry a divider)
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
    -> List (Entry a divider)
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
    -> List (Entry a divider)
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



---- CUSTOM DOM


{-| Create a customized view function for the DOM library of your choice by
providing some `DomFunctions`.
-}
customView :
    Internal.Views a divider attributeNever htmlNever
    -> DomFunctions attribute attributeNever html htmlNever msg
    -> CustomViewConfig a
    -> Instance a msg
    -> List (Entry a divider)
    -> Listbox
    -> List a
    -> html
customView views dom (CustomViewConfig config) =
    Internal.view True dom views config


{-| This record holds all the DOM functions needed to render a listbox. It is
probably instructive to look at the version for the standard `elm/html`
package:

    htmlFunctions : DomFunctions (Attribute msg) (Attribute Never) (Html msg) (Html Never) msg
    htmlFunctions =
        { ul = Html.ul
        , li = Html.li
        , on = Events.on
        , preventDefaultOn = Events.preventDefaultOn
        , property = Attributes.property
        , attribute = Attributes.attribute
        , attributeMap = \noOp -> Attributes.map (\_ -> noOp)
        , htmlMap = \noOp -> Html.map (\_ -> noOp)
        }

When using `mdgriffith/elm-ui`, you could define something like this:

    elementFunctions : DomFunctions (Attribute msg) (Attribute Never) (Element msg) (Element Never) msg
    elementFunctions =
        { ul = Element.column
        , li = Element.row
        , on =
            \event decoder -> Element.htmlAttribute (Events.on event decoder)
        , preventDefaultOn =
            \event decoder -> Element.htmlAttribute (Events.preventDefaultOn event decoder)
        , property =
            \name value -> Element.htmlAttribute (Attributes.property name value)
        , attribute =
            \name value -> Element.htmlAttribute (Attributes.attribute name value)
        , attributeMap = \noOp -> Element.mapAttribute (\_ -> noOp)
        , htmlMap = \noOp -> Element.map (\_ -> noOp)
        }

-}
type alias DomFunctions attribute attributeNever html htmlNever msg =
    { ul : List attribute -> List html -> html
    , li : List attribute -> List html -> html
    , on : String -> Decoder msg -> attribute
    , preventDefaultOn : String -> Decoder ( msg, Bool ) -> attribute
    , property : String -> Value -> attribute
    , attribute : String -> String -> attribute
    , attributeMap : msg -> attributeNever -> attribute
    , htmlMap : msg -> htmlNever -> html
    }


{-| -}
type CustomViewConfig a
    = CustomViewConfig (Internal.ViewConfig a)


{-| A replacement for `viewConfig` when you are using your own `customView`
function.
-}
customViewConfig :
    { uniqueId : a -> String
    , focusable : Bool
    , markActiveDescendant : Bool
    }
    -> CustomViewConfig a
customViewConfig =
    CustomViewConfig


{-| A replacement for `Views` when you are using your own `customView`
function. Take a look at its documentation for a description of each field.
-}
type alias CustomViews a divider attributeNever htmlNever =
    { ul : List attributeNever
    , liOption :
        { selected : Bool
        , focused : Bool
        , hovered : Bool
        , maybeQuery : Maybe String
        }
        -> a
        ->
            { attributes : List attributeNever
            , children : List htmlNever
            }
    , liDivider :
        divider
        ->
            { attributes : List attributeNever
            , children : List htmlNever
            }
    , empty : htmlNever
    , focusable : Bool
    , markActiveDescendant : Bool
    }


{-| A replacement for `focusedEntryId` when you are using your own `customView`
function. Take a look at its documentation for a description of each field.
-}
customFocusedEntryId :
    CustomViewConfig a
    -> Instance a msg
    -> List (Entry a divider)
    -> Listbox
    -> Maybe String
customFocusedEntryId (CustomViewConfig config) =
    Internal.focusedEntryId config


{-| A replacement for `preventDefaultOn` when you are using your own
`customView` function.
-}
customPreventDefaultOnKeyDown :
    (String -> Decoder ( msg, Bool ) -> attribute)
    -> Instance a msg
    -> Decoder ( msg, Bool )
    -> attribute
customPreventDefaultOnKeyDown =
    Internal.preventDefaultOnKeyDown



---- UPDATE


{-| The listbox's message type.
-}
type alias Msg a =
    Internal.Msg a


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
    -> List (Entry a divider)
    -> Msg a
    -> Listbox
    -> List a
    -> ( Listbox, Cmd (Msg a), List a )
update (UpdateConfig config) entries msg listbox selection =
    let
        ( newListbox, effect, newSelection ) =
            Internal.update config entries msg listbox selection
    in
    ( newListbox, perform effect, newSelection )


type alias InitialEntryDomData =
    { viewportList : Dom.Viewport
    , elementList : Dom.Element
    , elementLi : Dom.Element
    }


type alias EntryDomData =
    { viewportList : Dom.Viewport
    , elementList : Dom.Element
    , elementLi : Dom.Element
    , elementPreviousLi : Dom.Element
    }


perform : Internal.Effect a -> Cmd (Msg a)
perform effect =
    case effect of
        Internal.CmdNone ->
            Cmd.none

        Internal.TimeNow toMsg ->
            Task.perform toMsg Time.now

        Internal.DomFocus id ->
            Task.attempt (\_ -> Internal.NoOp) <|
                Dom.focus id

        -- SCROLLING
        Internal.GetViewportOfList toMsg id ->
            Dom.getViewportOf (Internal.printListId id)
                |> Task.attempt toMsg

        Internal.ScrollListToTop id ->
            Dom.getViewportOf (Internal.printListId id)
                |> Task.andThen
                    (\list ->
                        Dom.setViewportOf (Internal.printListId id)
                            list.viewport.x
                            0
                    )
                |> Task.attempt (\_ -> Internal.NoOp)

        Internal.ScrollListToBottom id ->
            Dom.getViewportOf (Internal.printListId id)
                |> Task.andThen
                    (\list ->
                        Dom.setViewportOf (Internal.printListId id)
                            list.viewport.x
                            list.scene.height
                    )
                |> Task.attempt (\_ -> Internal.NoOp)

        Internal.GetViewportOf toMsg behaviour id hash previousHash ->
            Task.map4 EntryDomData
                (Dom.getViewportOf (Internal.printListId id))
                (Dom.getElement (Internal.printListId id))
                (Dom.getElement (Internal.printEntryId id hash))
                (Dom.getElement (Internal.printEntryId id previousHash))
                |> Task.attempt toMsg

        Internal.SetViewportOf id x y ->
            Dom.setViewportOf (Internal.printListId id) x y
                |> Task.attempt (\_ -> Internal.NoOp)

        Internal.ScrollToOption behaviour id hash maybePreviousHash ->
            case maybePreviousHash of
                Nothing ->
                    Task.map3 InitialEntryDomData
                        (Dom.getViewportOf (Internal.printListId id))
                        (Dom.getElement (Internal.printListId id))
                        (Dom.getElement (Internal.printEntryId id hash))
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
                                        (Internal.printListId id)
                                        viewport.x
                                        (liY + liHeight / 2 - viewport.height / 2)

                                else
                                    Task.succeed ()
                            )
                        |> Task.attempt (\_ -> Internal.NoOp)

                Just previousHash ->
                    Task.map4 EntryDomData
                        (Dom.getViewportOf (Internal.printListId id))
                        (Dom.getElement (Internal.printListId id))
                        (Dom.getElement (Internal.printEntryId id hash))
                        (Dom.getElement (Internal.printEntryId id previousHash))
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
                                        Dom.setViewportOf (Internal.printListId id) x y
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
                        |> Task.attempt (\_ -> Internal.NoOp)



---- SUBSCRIPTIONS


{-| Do not forget to add this to your subscriptions:

    subscriptions model =
        Sub.map ListboxMsg
            (Listbox.subscriptions model.listbox)

-}
subscriptions : Listbox -> Sub (Msg a)
subscriptions listbox =
    Internal.subscriptions listbox
