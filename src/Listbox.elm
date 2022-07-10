module Listbox exposing
    ( view, Instance, Label, labelledBy, label, noLabel
    , Listbox, init
    , update, Msg, subscriptions
    , Behaviour
    , TypeAhead, noTypeAhead, simpleTypeAhead, typeAhead
    , Views, html
    , custom, ListboxAttrs, OptionAttrs
    , focusedEntry, focusedEntryId, hoveredEntry
    , focusEntry, focusNextOrFirstEntry, focusPreviousOrFirstEntry
    , focus
    , scrollToFocus
    , preventDefaultOnKeyDown
    )

{-| Implementation of the [listbox
widget](https://www.w3.org/TR/wai-aria-practices-1.1/#Listbox):

> A listbox widget presents a list of options and allows a user to select one
> or more of them.

Take a look at the documentation of `Behaviour` for the default keyboard
interactions this widget offers.


# View

@docs view, Instance, Label, labelledBy, label, noLabel


# State management

@docs Listbox, init
@docs update, Msg, subscriptions
@docs Behaviour


## Type-ahead

@docs TypeAhead, noTypeAhead, simpleTypeAhead, typeAhead


# View customization

@docs Views, html


## Advanced customization

@docs custom, ListboxAttrs, OptionAttrs


# Advanced usage


## State info

@docs focusedEntry, focusedEntryId, hoveredEntry


## State manipulation

@docs focusEntry, focusNextOrFirstEntry, focusPreviousOrFirstEntry


## DOM Stuff

@docs focus

@docs scrollToFocus

@docs preventDefaultOnKeyDown

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



---- MODEL


{-| Tracks the keyboard and mouse focus as well as the current query. The full
list of entries and the currently selected option(s) live in your own model.
-}
type Listbox
    = Listbox Data


type alias Data =
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


{-| Returns the option which currently has keyboard focus.
-}
focusedEntry : (a -> String) -> List a -> Listbox -> Maybe a
focusedEntry uniqueId allEntries (Listbox listbox) =
    Maybe.andThen (find uniqueId allEntries) (currentFocus listbox.focus)


{-| Returns the option which currently has mouse focus.
-}
hoveredEntry : (a -> String) -> List a -> Listbox -> Maybe a
hoveredEntry uniqueId allEntries (Listbox listbox) =
    Maybe.andThen (find uniqueId allEntries) listbox.hover


currentFocus : Focus -> Maybe String
currentFocus theFocus =
    case theFocus of
        NoFocus ->
            Nothing

        Focus current ->
            Just current

        Pending { current } ->
            Just current


{-| Returns the HTML id of the currently focused entry.
-}
focusedEntryId :
    { uniqueId : a -> String
    , focusable : Bool
    , markActiveDescendant : Bool
    }
    -> Instance a msg
    -> List a
    -> Listbox
    -> Maybe String
focusedEntryId config instance entries (Listbox listbox) =
    case listbox.focus of
        NoFocus ->
            Nothing

        Focus current ->
            find config.uniqueId entries current
                |> Maybe.map (printEntryId instance.id << config.uniqueId)

        Pending { current } ->
            find config.uniqueId entries current
                |> Maybe.map (printEntryId instance.id << config.uniqueId)



---- UPDATE CONFIG


{-| **Available behaviour customizations**

You can customize the behaviour of the listbox with the following options:

  - **jumpAtEnds**: Whether the keyboard focus should jump to the other end of
    the list when pressing `ArrowUp` while focusing the first option (or
    `ArrowDown` while focusing the last).

  - **separateFocus**: Whether the mouse focus and the keyboard focus can be
    different.

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
        , handleHomeAndEnd = True
        , typeAhead = simpleTypeAhead 300 identity
        , minimalGap = 30
        , initialGap = 200
        }

The listbox will behave as explained in the [WAI-ARIA Authoring Practices
1.1](https://www.w3.org/TR/wai-aria-practices-1.1/#Listbox) in the _Keyboard
Interaction_ section.

-}
type alias Behaviour a =
    { jumpAtEnds : Bool
    , separateFocus : Bool
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



---- UPDATE


{-| -}
type Msg a
    = NoOp
      -- FOCUS
    | ListFocused String
    | ListBlured
      -- MOUSE
    | ListMouseDown
    | ListMouseUp
    | EntryMouseEntered String
    | EntryMouseLeft
    | EntryClicked a
      -- ARROW KEYS
    | ListArrowUpDown String
    | ListShiftArrowUpDown String
    | ListArrowDownDown String
    | ListShiftArrowDownDown String
    | BrowserReturnedDomInfoOption a (Result Dom.Error DomInfoOption)
    | ViewportOfListReceived Direction a (Result Dom.Error Dom.Viewport)
      -- ENTER/SPACE
    | ListEnterDown String
    | ListSpaceDown String
    | ListShiftSpaceDown String
      -- HOME/END
    | ListHomeDown String
    | ListControlShiftHomeDown String
    | ListEndDown String
    | ListControlShiftEndDown String
      -- CTRL-A
    | ListControlADown
      -- QUERY
    | ListKeyDown String String
    | CurrentTimeReceived String String Time.Posix
    | Tick Time.Posix


type Direction
    = Up
    | Down


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

You can provide the following customizations:

  - **uniqueId**: A hash function for the entries.

  - **behaviour**: Behaviour customizations.

-}
update :
    { uniqueId : a -> String
    , behaviour : Behaviour a
    }
    -> List a
    -> Msg a
    -> Listbox
    -> List a
    -> ( Listbox, Cmd (Msg a), List a )
update config entries msg (Listbox data) selection =
    let
        ( newData, cmd, newSelection ) =
            updateHelp config entries msg data selection
    in
    ( Listbox newData, cmd, newSelection )


updateHelp :
    { uniqueId : a -> String
    , behaviour : Behaviour a
    }
    -> List a
    -> Msg a
    -> Data
    -> List a
    -> ( Data, Cmd (Msg a), List a )
updateHelp ({ uniqueId, behaviour } as config) allEntries msg data selection =
    case Debug.log "msg" msg of
        NoOp ->
            ( data, Cmd.none, selection )

        -- FOCUS
        ListFocused id ->
            if data.preventScroll then
                ( data, Cmd.none, selection )

            else
                initFocus config allEntries data selection id

        ListBlured ->
            ( { data
                | query = NoQuery
                , preventScroll = False
              }
            , Cmd.none
            , selection
            )

        -- MOUSE
        ListMouseDown ->
            ( { data | preventScroll = True }
            , Cmd.none
            , selection
            )

        ListMouseUp ->
            ( { data | preventScroll = False }
            , Cmd.none
            , selection
            )

        EntryMouseEntered newFocus ->
            ( { data
                | focus =
                    if behaviour.separateFocus then
                        data.focus

                    else
                        Focus newFocus
                , hover = Just newFocus
              }
            , Cmd.none
            , selection
            )

        EntryMouseLeft ->
            ( { data
                | hover =
                    if behaviour.separateFocus then
                        Nothing

                    else
                        data.hover
              }
            , Cmd.none
            , selection
            )

        EntryClicked a ->
            let
                hash =
                    uniqueId a
            in
            ( { data
                | query = NoQuery
                , focus = Focus hash
                , hover = Just hash
              }
            , Cmd.none
            , selection
            )
                |> toggle config a

        -- ARROW KEYS
        ListArrowUpDown id ->
            case data.focus of
                NoFocus ->
                    initFocus config allEntries data selection id

                Focus hash ->
                    scheduleFocusPrevious config allEntries data selection id False hash

                Pending _ ->
                    ( data, Cmd.none, selection )

        ListShiftArrowUpDown id ->
            case data.focus of
                NoFocus ->
                    initFocus config allEntries data selection id

                Focus hash ->
                    scheduleFocusPrevious config allEntries data selection id True hash

                Pending _ ->
                    ( data, Cmd.none, selection )

        ListArrowDownDown id ->
            case data.focus of
                NoFocus ->
                    initFocus config allEntries data selection id

                Focus hash ->
                    scheduleFocusNext config allEntries data selection id False hash

                Pending _ ->
                    ( data, Cmd.none, selection )

        ListShiftArrowDownDown id ->
            case data.focus of
                NoFocus ->
                    initFocus config allEntries data selection id

                Focus hash ->
                    scheduleFocusNext config allEntries data selection id True hash

                Pending _ ->
                    ( data, Cmd.none, selection )

        BrowserReturnedDomInfoOption a (Err id) ->
            ( data, Cmd.none, selection )

        BrowserReturnedDomInfoOption a (Ok entryDomData) ->
            case data.focus of
                NoFocus ->
                    ( data, Cmd.none, selection )

                Focus _ ->
                    ( data, Cmd.none, selection )

                Pending { id, pending, shiftDown } ->
                    let
                        newData =
                            { data | focus = Focus pending }

                        ( x, y ) =
                            newPosition behaviour entryDomData
                    in
                    if shiftDown then
                        ( newData
                        , setViewportOf id x y
                        , selection
                        )
                            |> toggle config a

                    else
                        ( newData
                        , setViewportOf id x y
                        , selection
                        )

        ViewportOfListReceived direction a (Err id) ->
            ( data, Cmd.none, selection )

        ViewportOfListReceived direction a (Ok viewport) ->
            case data.focus of
                NoFocus ->
                    ( data, Cmd.none, selection )

                Focus _ ->
                    ( data, Cmd.none, selection )

                Pending { id, pending, shiftDown } ->
                    let
                        newData =
                            { data | focus = Focus pending }

                        effect =
                            case direction of
                                Up ->
                                    setViewportOf id viewport.viewport.x viewport.scene.height

                                Down ->
                                    setViewportOf id viewport.viewport.x 0
                    in
                    if shiftDown then
                        ( newData
                        , effect
                        , selection
                        )
                            |> toggle config a

                    else
                        ( newData
                        , effect
                        , selection
                        )

        -- ENTER/SPACE
        ListEnterDown id ->
            case focusedEntry config.uniqueId allEntries (Listbox data) of
                Nothing ->
                    ( data, Cmd.none, selection )

                Just a ->
                    ( data, Cmd.none, selection )
                        |> toggle config a

        ListSpaceDown id ->
            case focusedEntry config.uniqueId allEntries (Listbox data) of
                Nothing ->
                    ( data, Cmd.none, selection )

                Just a ->
                    ( data, Cmd.none, selection )
                        |> toggle config a

        ListShiftSpaceDown id ->
            let
                selected =
                    Maybe.map2 (range uniqueId allEntries) (currentFocus data.focus) data.maybeLastSelectedEntry
                        |> Maybe.withDefault []
            in
            case selected of
                [] ->
                    ( data, Cmd.none, selection )

                a :: listA ->
                    ( data, Cmd.none, selection )
                        |> select config a listA

        -- HOME/END
        ListHomeDown id ->
            case List.head allEntries of
                Nothing ->
                    ( data, Cmd.none, selection )

                Just a ->
                    ( { data
                        | query = NoQuery
                        , focus = Focus (uniqueId a)
                      }
                    , scrollListToTop id
                    , selection
                    )

        ListControlShiftHomeDown id ->
            case Maybe.map uniqueId (List.head allEntries) of
                Nothing ->
                    ( data, Cmd.none, selection )

                Just hash ->
                    let
                        selected =
                            data.focus
                                |> currentFocus
                                |> Maybe.map (range uniqueId allEntries hash)
                                |> Maybe.withDefault []
                    in
                    case selected of
                        [] ->
                            ( data, Cmd.none, selection )

                        a :: listA ->
                            ( { data
                                | focus = Focus hash
                                , hover =
                                    if behaviour.separateFocus then
                                        data.hover

                                    else
                                        Just hash
                              }
                            , scrollListToTop id
                            , selection
                            )
                                |> select config a listA

        ListEndDown id ->
            case lastEntry allEntries of
                Nothing ->
                    ( data, Cmd.none, selection )

                Just a ->
                    ( { data
                        | query = NoQuery
                        , focus = Focus (uniqueId a)
                      }
                    , scrollListToBottom id
                    , selection
                    )

        ListControlShiftEndDown id ->
            case Maybe.map uniqueId (lastEntry allEntries) of
                Nothing ->
                    ( data, Cmd.none, selection )

                Just hash ->
                    let
                        selected =
                            data.focus
                                |> currentFocus
                                |> Maybe.map (range uniqueId allEntries hash)
                                |> Maybe.withDefault []
                    in
                    case selected of
                        [] ->
                            ( data, Cmd.none, selection )

                        a :: listA ->
                            ( { data
                                | focus = Focus hash
                                , hover =
                                    if behaviour.separateFocus then
                                        data.hover

                                    else
                                        Just hash
                              }
                            , scrollListToBottom id
                            , selection
                            )
                                |> select config a listA

        -- CTRL-A
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
                ( data
                , Cmd.none
                , []
                )

            else
                ( data
                , Cmd.none
                , allEntries
                )

        -- QUERY
        ListKeyDown id key ->
            case behaviour.typeAhead of
                NoTypeAhead ->
                    ( data, Cmd.none, selection )

                TypeAhead _ _ ->
                    ( data
                    , Task.perform (CurrentTimeReceived id key) Time.now
                    , selection
                    )

        CurrentTimeReceived id key currentTime ->
            case behaviour.typeAhead of
                NoTypeAhead ->
                    ( data, Cmd.none, selection )

                TypeAhead timeout matchesQuery ->
                    let
                        ( newQuery, queryText ) =
                            case data.query of
                                NoQuery ->
                                    ( Query timeout currentTime key, key )

                                Query _ _ query ->
                                    ( Query timeout currentTime (query ++ key), query ++ key )

                        maybeHash =
                            Maybe.andThen
                                (findWith matchesQuery uniqueId queryText allEntries)
                                (currentFocus data.focus)
                    in
                    case maybeHash of
                        Nothing ->
                            ( data, Cmd.none, selection )

                        Just hash ->
                            ( { data
                                | query = newQuery
                                , focus = Focus hash
                                , hover =
                                    if behaviour.separateFocus then
                                        data.hover

                                    else
                                        Just hash
                              }
                            , attemptToScrollToOption behaviour id hash Nothing
                            , selection
                            )

        Tick currentTime ->
            case data.query of
                NoQuery ->
                    ( data, Cmd.none, selection )

                Query timeout time _ ->
                    if
                        (Time.posixToMillis currentTime - Time.posixToMillis time)
                            > timeout
                    then
                        ( { data | query = NoQuery }
                        , Cmd.none
                        , selection
                        )

                    else
                        ( data, Cmd.none, selection )



-- FOCUS


initFocus :
    { uniqueId : a -> String
    , behaviour : Behaviour a
    }
    -> List a
    -> Data
    -> List a
    -> String
    -> ( Data, Cmd (Msg a), List a )
initFocus ({ uniqueId, behaviour } as config) allEntries data selection id =
    case
        data.focus
            |> currentFocus
            |> or data.maybeLastSelectedEntry
            |> Maybe.andThen (find uniqueId allEntries)
            |> or (List.head selection)
            |> Maybe.andThen (uniqueId >> find uniqueId allEntries)
            |> or (List.head allEntries)
    of
        Nothing ->
            ( { data | query = NoQuery }
            , Cmd.none
            , selection
            )

        Just a ->
            let
                hash =
                    uniqueId a
            in
            ( { data
                | query = NoQuery
                , focus = Focus hash
              }
            , attemptToScrollToOption behaviour id hash Nothing
            , selection
            )


scheduleFocusPrevious :
    { uniqueId : a -> String
    , behaviour : Behaviour a
    }
    -> List a
    -> Data
    -> List a
    -> String
    -> Bool
    -> String
    -> ( Data, Cmd (Msg a), List a )
scheduleFocusPrevious ({ uniqueId, behaviour } as config) allEntries data selection id shiftDown current =
    case findPrevious uniqueId allEntries current of
        Just (Last a) ->
            if behaviour.jumpAtEnds then
                ( { data
                    | query = NoQuery
                    , focus =
                        Pending
                            { id = id
                            , current = current
                            , pending = uniqueId a
                            , shiftDown = shiftDown
                            }
                  }
                , getViewportOfList id Up a
                , selection
                )

            else
                ( { data | query = NoQuery }
                , Cmd.none
                , selection
                )

        Just (Previous a) ->
            let
                hash =
                    uniqueId a
            in
            ( { data
                | query = NoQuery
                , focus =
                    Pending
                        { id = id
                        , current = current
                        , pending = hash
                        , shiftDown = shiftDown
                        }
              }
            , attemptToGetDomInfoOption id hash current a
            , selection
            )

        Nothing ->
            initFocus config allEntries data selection id


scheduleFocusNext :
    { uniqueId : a -> String
    , behaviour : Behaviour a
    }
    -> List a
    -> Data
    -> List a
    -> String
    -> Bool
    -> String
    -> ( Data, Cmd (Msg a), List a )
scheduleFocusNext ({ uniqueId, behaviour } as config) allEntries data selection id shiftDown current =
    case findNext uniqueId allEntries current of
        Just (First a) ->
            if behaviour.jumpAtEnds then
                ( { data
                    | query = NoQuery
                    , focus =
                        Pending
                            { id = id
                            , current = current
                            , pending = uniqueId a
                            , shiftDown = shiftDown
                            }
                  }
                , getViewportOfList id Down a
                , selection
                )

            else
                ( { data | query = NoQuery }
                , Cmd.none
                , selection
                )

        Just (Next a) ->
            let
                hash =
                    uniqueId a
            in
            ( { data
                | query = NoQuery
                , focus =
                    Pending
                        { id = id
                        , current = current
                        , pending = hash
                        , shiftDown = shiftDown
                        }
              }
            , attemptToGetDomInfoOption id hash current a
            , selection
            )

        Nothing ->
            initFocus config allEntries data selection id



-- SELECTION


select :
    { uniqueId : a -> String
    , behaviour : Behaviour a
    }
    -> a
    -> List a
    -> ( Data, Cmd (Msg a), List a )
    -> ( Data, Cmd (Msg a), List a )
select { uniqueId } a listA ( newData, effect, newSelection ) =
    ( { newData | maybeLastSelectedEntry = Just (uniqueId a) }
    , Cmd.none
    , List.uniqueBy uniqueId (a :: listA ++ newSelection)
    )


toggle :
    { uniqueId : a -> String
    , behaviour : Behaviour a
    }
    -> a
    -> ( Data, Cmd (Msg a), List a )
    -> ( Data, Cmd (Msg a), List a )
toggle { uniqueId } a ( newData, effect, newSelection ) =
    if List.member a newSelection then
        ( newData
        , Cmd.none
        , List.filter (\b -> a /= b) newSelection
        )

    else
        ( { newData | maybeLastSelectedEntry = Just (uniqueId a) }
        , Cmd.none
        , List.uniqueBy uniqueId (a :: newSelection)
        )


{-| Sets the keyboard focus to the provided options.

**Note**: This will not adjust the scroll position of the listbox, so you might
want to apply `scrollToFocus` afterwards.

-}
focusEntry :
    { uniqueId : a -> String
    , behaviour : Behaviour a
    }
    -> a
    -> Listbox
    -> List a
    -> ( Listbox, List a )
focusEntry config newEntry (Listbox listbox) selection =
    ( Listbox
        { listbox
            | query = NoQuery
            , focus = Focus (config.uniqueId newEntry)
        }
    , selection
    )


{-| Sets the keyboard focus to the next option. If `jumpAtEnds` is true and the
focus is already on the last option, the first option is selected.

**Note**: This will not adjust the scroll position of the listbox, so you might
want to apply `scrollToFocus` afterwards.

-}
focusNextOrFirstEntry :
    { uniqueId : a -> String
    , behaviour : Behaviour a
    }
    -> List a
    -> Listbox
    -> List a
    -> ( Listbox, List a )
focusNextOrFirstEntry config allEntries (Listbox listbox) selection =
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
            ( Listbox { listbox | focus = Focus (uniqueId a) }
            , selection
            )


{-| Sets the keyboard focus to the previous option. If `jumpAtEnds` is true and the
focus is already on the first option, the first option is selected.

**Note**: This will not adjust the scroll position of the listbox, so you might
want to apply `scrollToFocus` afterwards.

-}
focusPreviousOrFirstEntry :
    { uniqueId : a -> String
    , behaviour : Behaviour a
    }
    -> List a
    -> Listbox
    -> List a
    -> ( Listbox, List a )
focusPreviousOrFirstEntry config allEntries (Listbox listbox) selection =
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
            ( Listbox { listbox | focus = Focus (uniqueId a) }
            , selection
            )



-- CMDS


getViewportOfList : String -> Direction -> a -> Cmd (Msg a)
getViewportOfList id direction option =
    Dom.getViewportOf (printListId id)
        |> Task.attempt (ViewportOfListReceived direction option)


attemptToGetDomInfoOption : String -> String -> String -> a -> Cmd (Msg a)
attemptToGetDomInfoOption id hash previousHash option =
    getDomInfoOption id hash previousHash
        |> Task.attempt (BrowserReturnedDomInfoOption option)


scrollListToTop : String -> Cmd (Msg a)
scrollListToTop id =
    Dom.getViewportOf (printListId id)
        |> Task.andThen (\list -> Dom.setViewportOf (printListId id) list.viewport.x 0)
        |> Task.attempt (\_ -> NoOp)


scrollListToBottom : String -> Cmd (Msg a)
scrollListToBottom id =
    Dom.getViewportOf (printListId id)
        |> Task.andThen
            (\list ->
                Dom.setViewportOf (printListId id)
                    list.viewport.x
                    list.scene.height
            )
        |> Task.attempt (\_ -> NoOp)


attemptToScrollToOption : Behaviour msg -> String -> String -> Maybe String -> Cmd (Msg a)
attemptToScrollToOption behaviour id hash maybePreviousHash =
    case maybePreviousHash of
        Nothing ->
            getDomInfoOptionInitial id hash
                |> Task.andThen (scrollToOptionInitial behaviour id)
                |> Task.attempt (\_ -> NoOp)

        Just previousHash ->
            getDomInfoOption id hash previousHash
                |> Task.andThen (scrollToOption behaviour id)
                |> Task.attempt (\_ -> NoOp)


setViewportOf : String -> Float -> Float -> Cmd (Msg a)
setViewportOf id x y =
    Dom.setViewportOf (printListId id) x y
        |> Task.attempt (\_ -> NoOp)


{-| A command adjusting the scroll position of the listbox such that the
current keyboard focus is visible.
-}
scrollToFocus : Behaviour a -> Instance a msg -> Listbox -> Cmd msg
scrollToFocus behaviour { id, lift } (Listbox listbox) =
    Cmd.map lift
        (case listbox.focus of
            NoFocus ->
                Cmd.none

            Focus current ->
                attemptToScrollToOption behaviour id current Nothing

            Pending { current } ->
                attemptToScrollToOption behaviour id current Nothing
        )



-- TASKS


type alias DomInfoOptionInitial =
    { viewportList : Dom.Viewport
    , elementList : Dom.Element
    , elementLi : Dom.Element
    }


type alias DomInfoOption =
    { viewportList : Dom.Viewport
    , elementList : Dom.Element
    , elementLi : Dom.Element
    , elementPreviousLi : Dom.Element
    }


getDomInfoOptionInitial : String -> String -> Task Dom.Error DomInfoOptionInitial
getDomInfoOptionInitial id hash =
    Task.map3 DomInfoOptionInitial
        (Dom.getViewportOf (printListId id))
        (Dom.getElement (printListId id))
        (Dom.getElement (printEntryId id hash))


getDomInfoOption : String -> String -> String -> Task Dom.Error DomInfoOption
getDomInfoOption id hash previousHash =
    Task.map4 DomInfoOption
        (Dom.getViewportOf (printListId id))
        (Dom.getElement (printListId id))
        (Dom.getElement (printEntryId id hash))
        (Dom.getElement (printEntryId id previousHash))


scrollToOption : Behaviour msg -> String -> DomInfoOption -> Task Dom.Error ()
scrollToOption behaviour id entryDomData =
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


scrollToOptionInitial : Behaviour msg -> String -> DomInfoOptionInitial -> Task Dom.Error ()
scrollToOptionInitial behaviour id { viewportList, elementList, elementLi } =
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
        Dom.setViewportOf (printListId id) viewport.x (liY + liHeight / 2 - viewport.height / 2)

    else
        Task.succeed ()


{-| A task to give the listbox focus. The first argument must match the
`Instance` used in the `view` function!
-}
focus : Instance a msg -> Task Dom.Error ()
focus { id } =
    Dom.focus (printListId id)



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



---- VIEW CONFIG


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


{-| Opaque type for providing view customization of the listbox widget.
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


{-| TODO
-}
type alias ListboxAttrs msg =
    { id : String
    , role : String
    , ariaMultiselectable : String
    , ariaLabelledby : Maybe String
    , ariaLabel : Maybe String
    , ariaActivedescendant : Maybe String
    , tabindex : Maybe Int
    , preventDefaultOnKeydown : Decoder ( msg, Bool )
    , onMousedown : msg
    , onMouseup : msg
    , onFocus : msg
    , onBlur : msg
    }


{-| TODO
-}
type alias OptionAttrs msg =
    { id : String
    , role : String
    , ariaChecked : String
    , onMouseenter : msg
    , onMouseleave : msg
    , onClick : msg
    }


{-| If you want to use other UI libraries like `rtfeldman/elm-css` or
`mdgriffith/elm-ui` you have to generate Views using this function. Take a look
at the implementation of `html` for a starting point. The `examples/` folder of
the package repository contains an implementation for `mdgriffith/elm-ui`.
-}
custom :
    { listbox :
        ListboxAttrs msg
        ->
            { options : List node
            }
        -> node
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


{-| Generate view customizations for the standard `elm/html` package. You can
customize the styling with the following fields:

  - **ul**: A list of html attributes applied to the outer listbox.

  - **li**: A function returning `HtmlDetails` for each option in your
    entries list. It gets the actual option value `a` and flags telling you if
    this option is currently `selected`, `focused` or `hovered`. If the user
    typed in a query, you get this via the `maybeQuery` field.

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

                    addAriaLabel htmlAttrs =
                        case attrs.ariaLabel of
                            Nothing ->
                                htmlAttrs

                            Just ariaLabel ->
                                Attributes.attribute "aria-label" ariaLabel :: htmlAttrs

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
                        |> addAriaLabel
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
                in
                Html.li
                    ([ Attributes.id attrs.id
                     , Attributes.attribute "role" attrs.role
                     , Attributes.attribute "aria-checked" attrs.ariaChecked
                     , Events.onMouseEnter attrs.onMouseenter
                     , Events.onMouseLeave attrs.onMouseleave
                     , Events.onClick attrs.onClick
                     ]
                        ++ htmlDetails.attributes
                    )
                    htmlDetails.children
        }



---- VIEW


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

You can provide the following options:

  - **uniqueId**: A hash function for the entries.

  - **focusable**: Should the listbox be focusable?

-}
view :
    Views a node msg
    ->
        { uniqueId : a -> String
        , focusable : Bool
        , markActiveDescendant : Bool
        }
    -> Instance a msg
    -> List a
    -> Listbox
    -> List a
    -> node
view (Views views) config instance allEntries (Listbox data) selection =
    views.listbox
        { id = printListId instance.id
        , role = "listbox"
        , ariaMultiselectable = "true"
        , ariaLabelledby =
            case instance.label of
                LabelledBy id ->
                    Just id

                Label _ ->
                    Nothing

                NoLabel ->
                    Nothing
        , ariaLabel =
            case instance.label of
                LabelledBy _ ->
                    Nothing

                Label theLabel ->
                    Just theLabel

                NoLabel ->
                    Nothing
        , ariaActivedescendant =
            if config.markActiveDescendant then
                currentFocus data.focus
                    |> Maybe.andThen (find config.uniqueId allEntries)
                    |> Maybe.map (config.uniqueId >> printEntryId instance.id)

            else
                Nothing
        , tabindex =
            if config.focusable then
                Just 0

            else
                Nothing
        , preventDefaultOnKeydown =
            Decode.andThen
                (listKeyPress False instance.id
                    >> Decode.map (\msg -> ( instance.lift msg, True ))
                )
                KeyInfo.decoder
        , onMousedown = instance.lift ListMouseDown
        , onMouseup = instance.lift ListMouseUp
        , onFocus = instance.lift (ListFocused instance.id)
        , onBlur = instance.lift ListBlured
        }
        { options = List.map (viewOption views.option config instance selection data) allEntries
        }


viewOption :
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
    ->
        { uniqueId : a -> String
        , focusable : Bool
        , markActiveDescendant : Bool
        }
    -> Instance a msg
    -> List a
    -> Data
    -> a
    -> node
viewOption toNode config instance selection data option =
    let
        maybeHash =
            Just (config.uniqueId option)

        selected =
            List.any ((==) option) selection

        hash =
            config.uniqueId option
    in
    toNode
        { id = printEntryId instance.id hash
        , role = "option"
        , ariaChecked = stringFromBool selected
        , onMouseenter = instance.lift (EntryMouseEntered hash)
        , onMouseleave = instance.lift EntryMouseLeft
        , onClick = instance.lift (EntryClicked option)
        }
        { selected = selected
        , focused = currentFocus data.focus == maybeHash
        , hovered = data.hover == maybeHash
        , maybeQuery =
            case data.query of
                NoQuery ->
                    Nothing

                Query _ _ text ->
                    Just text
        }
        option


stringFromBool : Bool -> String
stringFromBool bool =
    if bool then
        "true"

    else
        "false"


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


{-| This adds all the keydown event listener needed for the listbox on any DOM
node. For example, this could be an input field which keeps focused while the
listbox is displayed in a dropdown. You usually want to set `focusable = False`
inside the `ViewConfig` when using this event listener.

You must provide your own event decoder, which is tried **before** the
listbox's event decoder. This lets you prevent the listbox reacting on key
combinations. If you do not need to handle keydown events, just insert a failing decoder:

    view =
        Html.input
            [ Html.Events.preventDefaultOn "keydown"
                (preventDefaultOnKeyDown
                    { id = "fruits-listbox"
                    , labelledBy = "fruits"
                    , lift = ListboxMsg
                    }
                    (Decode.fail "not handling this event here")
                )
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



---- IDS


printListId : String -> String
printListId id =
    id


printEntryId : String -> String -> String
printEntryId id entryId =
    id ++ "__element--" ++ entryId



---- HELP


newPosition : Behaviour a -> DomInfoOption -> ( Float, Float )
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



---- MISC


or : Maybe a -> Maybe a -> Maybe a
or fallback default =
    case default of
        Nothing ->
            fallback

        Just _ ->
            default


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



---- FIND


find : (a -> String) -> List a -> String -> Maybe a
find uniqueId entries selectedId =
    List.find (\entry -> uniqueId entry == selectedId) entries


findWith : (String -> a -> Bool) -> (a -> String) -> String -> List a -> String -> Maybe String
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


proceedWith : (String -> a -> Bool) -> (a -> String) -> String -> String -> List a -> Maybe String
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



---- PREVIOUS


type Previous a
    = Previous a
    | Last a


findPrevious : (a -> String) -> List a -> String -> Maybe (Previous a)
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


findPreviousHelp : a -> (a -> String) -> List a -> String -> Maybe (Previous a)
findPreviousHelp previous uniqueId entries currentId =
    case entries of
        [] ->
            Nothing

        first :: rest ->
            if uniqueId first == currentId then
                Just (Previous previous)

            else
                findPreviousHelp first uniqueId rest currentId



---- NEXT


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


findNextHelp : a -> (a -> String) -> List a -> String -> Next a
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
