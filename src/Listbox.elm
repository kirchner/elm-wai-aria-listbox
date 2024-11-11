module Listbox exposing
    ( view, Instance, Label, labelledBy, label, noLabel
    , Listbox, init
    , update, Msg, subscriptions
    , Behaviour
    , TypeAhead, noTypeAhead, simpleTypeAhead, typeAhead
    , Views, html
    , custom, ListboxAttrs, OptionAttrs
    , optionActive, optionActiveId, optionHovered
    , activateOption, activateNextOrFirstOption, activatePreviousOrFirstOption
    , focus
    , scrollToActiveOption
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

@docs optionActive, optionActiveId, optionHovered


## State manipulation

@docs activateOption, activateNextOrFirstOption, activatePreviousOrFirstOption


## DOM Stuff

@docs focus

@docs scrollToActiveOption

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
import Process
import Set
import Task exposing (Task)
import Time exposing (Posix)



---- MODEL


{-| Tracks the keyboard and mouse focus as well as the current query. The full
list of options and the currently selected option(s) live in your own model.
-}
type Listbox
    = Listbox Data


type alias Data =
    { mousePressed : Bool
    , query : Query
    , optionActive : ActiveOption
    , optionHovered : Maybe String
    , optionSelectedLast : Maybe String
    }


type Query
    = NoQuery
    | Query Int Time.Posix String


type ActiveOption
    = NoActiveOption
    | ActivatingOption
        { id : String
        , optionCurrent : String
        , optionPending : String
        , shiftDown : Bool
        }
    | ActiveOption String


optionActiveUniqueId : ActiveOption -> Maybe String
optionActiveUniqueId theFocus =
    case theFocus of
        NoActiveOption ->
            Nothing

        ActivatingOption { optionCurrent } ->
            Just optionCurrent

        ActiveOption current ->
            Just current


{-| An initial listbox with no option focused.
-}
init : Listbox
init =
    Listbox
        { mousePressed = False
        , query = NoQuery
        , optionActive = NoActiveOption
        , optionHovered = Nothing
        , optionSelectedLast = Nothing
        }


{-| Returns the option which currently has keyboard focus.
-}
optionActive : (a -> String) -> List a -> Listbox -> Maybe a
optionActive uniqueId alloptions (Listbox listbox) =
    Maybe.andThen (find uniqueId alloptions) (optionActiveUniqueId listbox.optionActive)


{-| Returns the option which currently has mouse focus.
-}
optionHovered : (a -> String) -> List a -> Listbox -> Maybe a
optionHovered uniqueId alloptions (Listbox listbox) =
    Maybe.andThen (find uniqueId alloptions) listbox.optionHovered


{-| Returns the HTML id of the currently focused option.
-}
optionActiveId :
    { uniqueId : a -> String
    , focusable : Bool
    , markActiveDescendant : Bool
    }
    -> Instance a msg
    -> List a
    -> Listbox
    -> Maybe String
optionActiveId config instance options (Listbox listbox) =
    case listbox.optionActive of
        NoActiveOption ->
            Nothing

        ActivatingOption { optionCurrent } ->
            find config.uniqueId options optionCurrent
                |> Maybe.map (printOptionId instance.id << config.uniqueId)

        ActiveOption current ->
            find config.uniqueId options current
                |> Maybe.map (printOptionId instance.id << config.uniqueId)



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
simpleTypeAhead timeout optionToString =
    TypeAhead timeout <|
        \query a ->
            String.toLower (optionToString a)
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
    | UserFocusedList String
    | UserBluredList
      -- MOUSE
    | UserPressedMouse
    | UserReleasedMouse
    | UserHoveredOption String
    | UserLeftOption
    | UserClickedOption a
      -- ARROW KEYS
    | UserPressedArrowUp String
    | UserPressedArrowUpWithShift String
    | UserPressedArrowDown String
    | UserPressedArrowDownWithShift String
    | BrowserReturnedDomInfoOption a (Result Dom.Error DomInfoOption)
    | BrowserReturnedViewportOfList Direction a (Result Dom.Error Dom.Viewport)
      -- ENTER/SPACE
    | UserPressedEnter String
    | UserPressedSpace String
    | UserPressedSpaceWithShift String
      -- HOME/END
    | UserPressedHome String
    | UserPressedHomeWithControlShift String
    | UserPressedEnd String
    | UserPressedEndWithControlShift String
      -- CTRL-A
    | UserPressedAWithControl
      -- QUERY
    | UserPressedKey String String
    | BrowserReturnedCurrentTime String String Time.Posix
    | BrowserSentTick Time.Posix


type Direction
    = Up
    | Down


{-| Use this function to update the listbox state. You have to provide the same
options and selection as given to your view function.

For example:

    update msg model =
        case msg of
            ListboxMsg listboxMsg ->
                let
                    ( newListbox, listboxCmd, newSelection ) =
                        Listbox.update updateConfig
                            options
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

In a more sophisticated example, the options could be dynamic, as well. (For
example, loaded via an HTTP request.)

You can provide the following customizations:

  - **uniqueId**: A hash function for the options.

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
update config options msg (Listbox data) selection =
    let
        ( newData, cmd, newSelection ) =
            updateHelp config options msg data selection
    in
    ( Listbox newData, cmd, newSelection )


type alias ConfigUpdate a =
    { uniqueId : a -> String
    , behaviour : Behaviour a
    }


updateHelp : ConfigUpdate a -> List a -> Msg a -> Data -> List a -> ( Data, Cmd (Msg a), List a )
updateHelp ({ uniqueId, behaviour } as config) alloptions msg data selection =
    case msg of
        NoOp ->
            ( data, Cmd.none, selection )

        -- FOCUS
        UserFocusedList id ->
            if data.mousePressed then
                ( data, Cmd.none, selection )

            else
                activateOptionInitial config alloptions data selection id

        UserBluredList ->
            ( { data
                | query = NoQuery
                , mousePressed = False
              }
            , Cmd.none
            , selection
            )

        -- MOUSE
        UserPressedMouse ->
            ( { data | mousePressed = True }
            , Cmd.none
            , selection
            )

        UserReleasedMouse ->
            ( { data | mousePressed = False }
            , Cmd.none
            , selection
            )

        UserHoveredOption newFocus ->
            ( { data
                | optionActive =
                    if behaviour.separateFocus then
                        data.optionActive

                    else
                        ActiveOption newFocus
                , optionHovered = Just newFocus
              }
            , Cmd.none
            , selection
            )

        UserLeftOption ->
            ( { data
                | optionHovered =
                    if behaviour.separateFocus then
                        Nothing

                    else
                        data.optionHovered
              }
            , Cmd.none
            , selection
            )

        UserClickedOption a ->
            let
                hash =
                    uniqueId a
            in
            ( { data
                | query = NoQuery
                , optionActive = ActiveOption hash
                , optionHovered = Just hash
              }
            , Cmd.none
            , selection
            )
                |> toggleOption config a

        -- ARROW KEYS
        UserPressedArrowUp id ->
            activateOptionPrevious False config alloptions data selection id

        UserPressedArrowUpWithShift id ->
            activateOptionPrevious True config alloptions data selection id

        UserPressedArrowDown id ->
            activateOptionNext False config alloptions data selection id

        UserPressedArrowDownWithShift id ->
            activateOptionNext True config alloptions data selection id

        BrowserReturnedDomInfoOption a (Err id) ->
            ( data, Cmd.none, selection )

        BrowserReturnedDomInfoOption a (Ok optionDomData) ->
            case data.optionActive of
                NoActiveOption ->
                    ( data, Cmd.none, selection )

                ActivatingOption { id, optionPending, shiftDown } ->
                    let
                        newData =
                            { data | optionActive = ActiveOption optionPending }

                        ( x, y ) =
                            newPosition behaviour optionDomData
                    in
                    if shiftDown then
                        ( newData
                        , setViewportOf id x y
                        , selection
                        )
                            |> toggleOption config a

                    else
                        ( newData
                        , setViewportOf id x y
                        , selection
                        )

                ActiveOption _ ->
                    ( data, Cmd.none, selection )

        BrowserReturnedViewportOfList direction a (Err id) ->
            ( data, Cmd.none, selection )

        BrowserReturnedViewportOfList direction a (Ok viewport) ->
            case data.optionActive of
                NoActiveOption ->
                    ( data, Cmd.none, selection )

                ActivatingOption { id, optionPending, shiftDown } ->
                    let
                        newData =
                            { data | optionActive = ActiveOption optionPending }

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
                            |> toggleOption config a

                    else
                        ( newData
                        , effect
                        , selection
                        )

                ActiveOption _ ->
                    ( data, Cmd.none, selection )

        -- ENTER/SPACE
        UserPressedEnter id ->
            case optionActive config.uniqueId alloptions (Listbox data) of
                Nothing ->
                    ( data, Cmd.none, selection )

                Just a ->
                    ( data, Cmd.none, selection )
                        |> toggleOption config a

        UserPressedSpace id ->
            case optionActive config.uniqueId alloptions (Listbox data) of
                Nothing ->
                    ( data, Cmd.none, selection )

                Just a ->
                    ( data, Cmd.none, selection )
                        |> toggleOption config a

        UserPressedSpaceWithShift id ->
            let
                selected =
                    Maybe.map2 (range uniqueId alloptions) (optionActiveUniqueId data.optionActive) data.optionSelectedLast
                        |> Maybe.withDefault []
            in
            case selected of
                [] ->
                    ( data, Cmd.none, selection )

                a :: listA ->
                    ( data, Cmd.none, selection )
                        |> selectOption config a listA

        -- HOME/END
        UserPressedHome id ->
            case List.head alloptions of
                Nothing ->
                    ( data, Cmd.none, selection )

                Just a ->
                    ( { data
                        | query = NoQuery
                        , optionActive = ActiveOption (uniqueId a)
                      }
                    , scrollListToTop id
                    , selection
                    )

        UserPressedHomeWithControlShift id ->
            case Maybe.map uniqueId (List.head alloptions) of
                Nothing ->
                    ( data, Cmd.none, selection )

                Just hash ->
                    let
                        selected =
                            data.optionActive
                                |> optionActiveUniqueId
                                |> Maybe.map (range uniqueId alloptions hash)
                                |> Maybe.withDefault []
                    in
                    case selected of
                        [] ->
                            ( data, Cmd.none, selection )

                        a :: listA ->
                            ( { data
                                | optionActive = ActiveOption hash
                                , optionHovered =
                                    if behaviour.separateFocus then
                                        data.optionHovered

                                    else
                                        Just hash
                              }
                            , scrollListToTop id
                            , selection
                            )
                                |> selectOption config a listA

        UserPressedEnd id ->
            case lastOption alloptions of
                Nothing ->
                    ( data, Cmd.none, selection )

                Just a ->
                    ( { data
                        | query = NoQuery
                        , optionActive = ActiveOption (uniqueId a)
                      }
                    , scrollListToBottom id
                    , selection
                    )

        UserPressedEndWithControlShift id ->
            case Maybe.map uniqueId (lastOption alloptions) of
                Nothing ->
                    ( data, Cmd.none, selection )

                Just hash ->
                    let
                        selected =
                            data.optionActive
                                |> optionActiveUniqueId
                                |> Maybe.map (range uniqueId alloptions hash)
                                |> Maybe.withDefault []
                    in
                    case selected of
                        [] ->
                            ( data, Cmd.none, selection )

                        a :: listA ->
                            ( { data
                                | optionActive = ActiveOption hash
                                , optionHovered =
                                    if behaviour.separateFocus then
                                        data.optionHovered

                                    else
                                        Just hash
                              }
                            , scrollListToBottom id
                            , selection
                            )
                                |> selectOption config a listA

        -- CTRL-A
        UserPressedAWithControl ->
            let
                alloptionsSet =
                    alloptions
                        |> List.map uniqueId
                        |> Set.fromList

                selectionSet =
                    selection
                        |> List.map uniqueId
                        |> Set.fromList
            in
            if Set.isEmpty (Set.diff alloptionsSet selectionSet) then
                ( data
                , Cmd.none
                , []
                )

            else
                ( data
                , Cmd.none
                , alloptions
                )

        -- QUERY
        UserPressedKey id key ->
            case behaviour.typeAhead of
                NoTypeAhead ->
                    ( data, Cmd.none, selection )

                TypeAhead _ _ ->
                    ( data
                    , Task.perform (BrowserReturnedCurrentTime id key) Time.now
                    , selection
                    )

        BrowserReturnedCurrentTime id key currentTime ->
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
                                (findWith matchesQuery uniqueId queryText alloptions)
                                (optionActiveUniqueId data.optionActive)
                    in
                    case maybeHash of
                        Nothing ->
                            ( data, Cmd.none, selection )

                        Just hash ->
                            ( { data
                                | query = newQuery
                                , optionActive = ActiveOption hash
                                , optionHovered =
                                    if behaviour.separateFocus then
                                        data.optionHovered

                                    else
                                        Just hash
                              }
                            , attemptToScrollToOption behaviour id hash Nothing
                            , selection
                            )

        BrowserSentTick currentTime ->
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


activateOptionInitial : ConfigUpdate a -> List a -> Data -> List a -> String -> ( Data, Cmd (Msg a), List a )
activateOptionInitial ({ uniqueId, behaviour } as config) alloptions data selection id =
    case
        data.optionActive
            |> optionActiveUniqueId
            |> or data.optionSelectedLast
            |> Maybe.andThen (find uniqueId alloptions)
            |> or (List.head selection)
            |> Maybe.andThen (uniqueId >> find uniqueId alloptions)
            |> or (List.head alloptions)
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
                , optionActive = ActiveOption hash
              }
            , attemptToScrollToOption behaviour id hash Nothing
            , selection
            )


activateOptionPrevious : Bool -> ConfigUpdate a -> List a -> Data -> List a -> String -> ( Data, Cmd (Msg a), List a )
activateOptionPrevious shiftDown ({ uniqueId, behaviour } as config) alloptions data selection id =
    case data.optionActive of
        NoActiveOption ->
            activateOptionInitial config alloptions data selection id

        ActivatingOption _ ->
            ( data, Cmd.none, selection )

        ActiveOption hashCurrent ->
            case findPrevious uniqueId alloptions hashCurrent of
                Just (Last a) ->
                    if behaviour.jumpAtEnds then
                        ( { data
                            | query = NoQuery
                            , optionActive =
                                ActivatingOption
                                    { id = id
                                    , optionCurrent = hashCurrent
                                    , optionPending = uniqueId a
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
                        hashActivatingOption =
                            uniqueId a
                    in
                    ( { data
                        | query = NoQuery
                        , optionActive =
                            ActivatingOption
                                { id = id
                                , optionCurrent = hashCurrent
                                , optionPending = hashActivatingOption
                                , shiftDown = shiftDown
                                }
                      }
                    , attemptToGetDomInfoOption id hashActivatingOption hashCurrent a
                    , selection
                    )

                Nothing ->
                    activateOptionInitial config alloptions data selection id


activateOptionNext : Bool -> ConfigUpdate a -> List a -> Data -> List a -> String -> ( Data, Cmd (Msg a), List a )
activateOptionNext shiftDown ({ uniqueId, behaviour } as config) alloptions data selection id =
    case data.optionActive of
        NoActiveOption ->
            activateOptionInitial config alloptions data selection id

        ActivatingOption _ ->
            ( data, Cmd.none, selection )

        ActiveOption hashCurrent ->
            case findNext uniqueId alloptions hashCurrent of
                Just (First a) ->
                    if behaviour.jumpAtEnds then
                        ( { data
                            | query = NoQuery
                            , optionActive =
                                ActivatingOption
                                    { id = id
                                    , optionCurrent = hashCurrent
                                    , optionPending = uniqueId a
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
                        hashActivatingOption =
                            uniqueId a
                    in
                    ( { data
                        | query = NoQuery
                        , optionActive =
                            ActivatingOption
                                { id = id
                                , optionCurrent = hashCurrent
                                , optionPending = hashActivatingOption
                                , shiftDown = shiftDown
                                }
                      }
                    , attemptToGetDomInfoOption id hashActivatingOption hashCurrent a
                    , selection
                    )

                Nothing ->
                    activateOptionInitial config alloptions data selection id



-- SELECTION


selectOption : ConfigUpdate a -> a -> List a -> ( Data, Cmd (Msg a), List a ) -> ( Data, Cmd (Msg a), List a )
selectOption { uniqueId } a listA ( newData, effect, newSelection ) =
    ( { newData | optionSelectedLast = Just (uniqueId a) }
    , effect
    , List.uniqueBy uniqueId (a :: listA ++ newSelection)
    )


toggleOption : ConfigUpdate a -> a -> ( Data, Cmd (Msg a), List a ) -> ( Data, Cmd (Msg a), List a )
toggleOption { uniqueId } a ( newData, effect, newSelection ) =
    if List.member (uniqueId a) (List.map uniqueId newSelection) then
        ( newData
        , effect
        , List.filter (\b -> uniqueId a /= uniqueId b) newSelection
        )

    else
        ( { newData | optionSelectedLast = Just (uniqueId a) }
        , effect
        , List.uniqueBy uniqueId (a :: newSelection)
        )


{-| Sets the keyboard focus to the provided options.

**Note**: This will not adjust the scroll position of the listbox, so you might
want to apply `scrollToActiveOption` afterwards.

-}
activateOption :
    { uniqueId : a -> String
    , behaviour : Behaviour a
    }
    -> a
    -> Listbox
    -> List a
    -> ( Listbox, List a )
activateOption config newOption (Listbox listbox) selection =
    ( Listbox
        { listbox
            | query = NoQuery
            , optionActive = ActiveOption (config.uniqueId newOption)
        }
    , selection
    )


{-| Sets the keyboard focus to the next option. If `jumpAtEnds` is true and the
focus is already on the last option, the first option is selected.

**Note**: This will not adjust the scroll position of the listbox, so you might
want to apply `scrollToActiveOption` afterwards.

-}
activateNextOrFirstOption :
    { uniqueId : a -> String
    , behaviour : Behaviour a
    }
    -> List a
    -> Listbox
    -> List a
    -> ( Listbox, List a )
activateNextOrFirstOption config alloptions (Listbox listbox) selection =
    let
        { uniqueId, behaviour } =
            config

        maybeA =
            case optionActiveUniqueId listbox.optionActive of
                Nothing ->
                    List.head alloptions

                Just hash ->
                    case findNext uniqueId alloptions hash of
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
            ( Listbox { listbox | optionActive = ActiveOption (uniqueId a) }
            , selection
            )


{-| Sets the keyboard focus to the previous option. If `jumpAtEnds` is true and the
focus is already on the first option, the first option is selected.

**Note**: This will not adjust the scroll position of the listbox, so you might
want to apply `scrollToActiveOption` afterwards.

-}
activatePreviousOrFirstOption :
    { uniqueId : a -> String
    , behaviour : Behaviour a
    }
    -> List a
    -> Listbox
    -> List a
    -> ( Listbox, List a )
activatePreviousOrFirstOption config alloptions (Listbox listbox) selection =
    let
        { uniqueId, behaviour } =
            config

        maybeA =
            case optionActiveUniqueId listbox.optionActive of
                Nothing ->
                    List.head alloptions

                Just hash ->
                    case findPrevious uniqueId alloptions hash of
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
            ( Listbox { listbox | optionActive = ActiveOption (uniqueId a) }
            , selection
            )



-- CMDS


getViewportOfList : String -> Direction -> a -> Cmd (Msg a)
getViewportOfList id direction option =
    Process.sleep 0
        |> Task.andThen (\_ -> Dom.getViewportOf (printListId id))
        |> Task.attempt (BrowserReturnedViewportOfList direction option)


attemptToGetDomInfoOption : String -> String -> String -> a -> Cmd (Msg a)
attemptToGetDomInfoOption id hash previousHash option =
    Process.sleep 0
        |> Task.andThen (\_ -> getDomInfoOption id hash previousHash)
        |> Task.attempt (BrowserReturnedDomInfoOption option)


scrollListToTop : String -> Cmd (Msg a)
scrollListToTop id =
    Process.sleep 0
        |> Task.andThen (\_ -> Dom.getViewportOf (printListId id))
        |> Task.andThen (\list -> Dom.setViewportOf (printListId id) list.viewport.x 0)
        |> Task.attempt (\_ -> NoOp)


scrollListToBottom : String -> Cmd (Msg a)
scrollListToBottom id =
    Process.sleep 0
        |> Task.andThen (\_ -> Dom.getViewportOf (printListId id))
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
            Process.sleep 0
                |> Task.andThen (\_ -> getDomInfoOptionInitial id hash)
                |> Task.andThen (scrollToOptionInitial behaviour id)
                |> Task.attempt (\_ -> NoOp)

        Just previousHash ->
            Process.sleep 0
                |> Task.andThen (\_ -> getDomInfoOption id hash previousHash)
                |> Task.andThen (scrollToOption behaviour id)
                |> Task.attempt (\_ -> NoOp)


setViewportOf : String -> Float -> Float -> Cmd (Msg a)
setViewportOf id x y =
    Process.sleep 0
        |> Task.andThen (\_ -> Dom.setViewportOf (printListId id) x y)
        |> Task.attempt (\_ -> NoOp)


{-| A command adjusting the scroll position of the listbox such that the
current keyboard focus is visible.
-}
scrollToActiveOption : Behaviour a -> Instance a msg -> Listbox -> Cmd msg
scrollToActiveOption behaviour { id, lift } (Listbox listbox) =
    Cmd.map lift
        (case listbox.optionActive of
            NoActiveOption ->
                Cmd.none

            ActiveOption current ->
                attemptToScrollToOption behaviour id current Nothing

            ActivatingOption { optionCurrent } ->
                attemptToScrollToOption behaviour id optionCurrent Nothing
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
        (Dom.getElement (printOptionId id hash))


getDomInfoOption : String -> String -> String -> Task Dom.Error DomInfoOption
getDomInfoOption id hash previousHash =
    Task.map4 DomInfoOption
        (Dom.getViewportOf (printListId id))
        (Dom.getElement (printListId id))
        (Dom.getElement (printOptionId id hash))
        (Dom.getElement (printOptionId id previousHash))


scrollToOption : Behaviour msg -> String -> DomInfoOption -> Task Dom.Error ()
scrollToOption behaviour id optionDomData =
    let
        viewport =
            optionDomData.viewportList.viewport

        list =
            optionDomData.elementList

        li =
            optionDomData.elementLi

        previousLi =
            optionDomData.elementPreviousLi

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
        previousOptionHidden =
            (previousLiY + previousLiHeight < viewport.y)
                || (previousLiY > viewport.y + viewport.height)

        newOptionTooLow =
            liY + liHeight + behaviour.minimalGap > viewport.y + viewport.height

        newOptionTooHigh =
            liY - behaviour.minimalGap < viewport.y

        -- EFFECT
        centerNewOption =
            domSetViewportOf viewport.x <|
                (liY + liHeight / 2 - viewport.height / 2)

        scrollDownToNewOption =
            domSetViewportOf viewport.x <|
                (liY + liHeight - viewport.height + behaviour.initialGap)

        scrollUpToNewOption =
            domSetViewportOf viewport.x <|
                (liY - behaviour.initialGap)

        domSetViewportOf x y =
            Dom.setViewportOf (printListId id) x y
    in
    if previousOptionHidden then
        centerNewOption

    else if newOptionTooLow then
        scrollDownToNewOption

    else if newOptionTooHigh then
        scrollUpToNewOption

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

        optionHidden =
            (liY + liHeight - behaviour.minimalGap < viewport.y)
                || (liY + behaviour.minimalGap > viewport.y + viewport.height)
    in
    if optionHidden then
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
            Time.every (toFloat (timeout // 3)) BrowserSentTick



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
    options list. It gets the actual option value `a` and flags telling you if
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


{-| Take a list of all options and a list of selected options and display it as
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

    fruits : List (Option String divider)
    fruits =
        List.map Listbox.option
            [ "Apple", "Banana", "Cherry" ]

    type Msg
        = ListboxMsg Listbox.Msg

You can provide the following options:

  - **uniqueId**: A hash function for the options.

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
view (Views views) config instance alloptions (Listbox data) selection =
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
                optionActiveUniqueId data.optionActive
                    |> Maybe.andThen (find config.uniqueId alloptions)
                    |> Maybe.map (config.uniqueId >> printOptionId instance.id)

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
        , onMousedown = instance.lift UserPressedMouse
        , onMouseup = instance.lift UserReleasedMouse
        , onFocus = instance.lift (UserFocusedList instance.id)
        , onBlur = instance.lift UserBluredList
        }
        { options = List.map (viewOption views.option config instance selection data) alloptions
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
            List.any (\otherOption -> config.uniqueId otherOption == config.uniqueId option) selection

        hash =
            config.uniqueId option
    in
    toNode
        { id = printOptionId instance.id hash
        , role = "option"
        , ariaChecked = stringFromBool selected
        , onMouseenter = instance.lift (UserHoveredOption hash)
        , onMouseleave = instance.lift UserLeftOption
        , onClick = instance.lift (UserClickedOption option)
        }
        { selected = selected
        , focused = optionActiveUniqueId data.optionActive == maybeHash
        , hovered = data.optionHovered == maybeHash
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
                Decode.succeed (UserPressedArrowUp id)

            else if onlyShiftDown then
                Decode.succeed (UserPressedArrowUpWithShift id)

            else
                notHandlingThatKey

        "ArrowDown" ->
            if noModifierDown then
                Decode.succeed (UserPressedArrowDown id)

            else if onlyShiftDown then
                Decode.succeed (UserPressedArrowDownWithShift id)

            else
                notHandlingThatKey

        "Enter" ->
            if noModifierDown then
                Decode.succeed (UserPressedEnter id)

            else
                notHandlingThatKey

        " " ->
            if not fromOutside then
                if onlyShiftDown then
                    Decode.succeed (UserPressedSpaceWithShift id)

                else if noModifierDown then
                    Decode.succeed (UserPressedSpace id)

                else
                    notHandlingThatKey

            else
                notHandlingThatKey

        "Home" ->
            if not altDown && controlDown && not metaDown && shiftDown then
                Decode.succeed (UserPressedHomeWithControlShift id)

            else if noModifierDown then
                Decode.succeed (UserPressedHome id)

            else
                notHandlingThatKey

        "End" ->
            if not altDown && controlDown && not metaDown && shiftDown then
                Decode.succeed (UserPressedEndWithControlShift id)

            else if noModifierDown then
                Decode.succeed (UserPressedEnd id)

            else
                notHandlingThatKey

        "a" ->
            if not fromOutside then
                if onlyControlDown then
                    Decode.succeed UserPressedAWithControl

                else if noModifierDown && (String.length code == 1) then
                    Decode.succeed (UserPressedKey id code)

                else
                    notHandlingThatKey

            else
                notHandlingThatKey

        _ ->
            if not fromOutside then
                if noModifierDown && (String.length code == 1) then
                    Decode.succeed (UserPressedKey id code)

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
    id ++ "__listbox"


printOptionId : String -> String -> String
printOptionId id optionId =
    id ++ "__listbox__element--" ++ optionId



---- HELP


newPosition : Behaviour a -> DomInfoOption -> ( Float, Float )
newPosition behaviour optionDomData =
    let
        ---- SCROLLING
        viewport =
            optionDomData.viewportList.viewport

        list =
            optionDomData.elementList

        li =
            optionDomData.elementLi

        previousLi =
            optionDomData.elementPreviousLi

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
        previousOptionHidden =
            (previousLiY + previousLiHeight < viewport.y)
                || (previousLiY > viewport.y + viewport.height)

        newOptionTooLow =
            liY + liHeight + behaviour.minimalGap > viewport.y + viewport.height

        newOptionTooHigh =
            liY - behaviour.minimalGap < viewport.y
    in
    if previousOptionHidden then
        ( viewport.x
        , liY + liHeight / 2 - viewport.height / 2
        )

    else if newOptionTooLow then
        ( viewport.x
        , liY + liHeight - viewport.height + behaviour.initialGap
        )

    else if newOptionTooHigh then
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
find uniqueId options selectedId =
    List.find (\option -> uniqueId option == selectedId) options


findWith : (String -> a -> Bool) -> (a -> String) -> String -> List a -> String -> Maybe String
findWith matchesQuery uniqueId query options id =
    case options of
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
proceedWith matchesQuery uniqueId id query options =
    case options of
        [] ->
            Just id

        a :: rest ->
            if matchesQuery query a then
                Just (uniqueId a)

            else
                proceedWith matchesQuery uniqueId id query rest


lastOption : List a -> Maybe a
lastOption options =
    List.head (List.reverse options)



---- PREVIOUS


type Previous a
    = Previous a
    | Last a


findPrevious : (a -> String) -> List a -> String -> Maybe (Previous a)
findPrevious uniqueId options currentId =
    case options of
        [] ->
            Nothing

        first :: rest ->
            if uniqueId first == currentId then
                options
                    |> lastOption
                    |> Maybe.map Last

            else
                findPreviousHelp first uniqueId rest currentId


findPreviousHelp : a -> (a -> String) -> List a -> String -> Maybe (Previous a)
findPreviousHelp previous uniqueId options currentId =
    case options of
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
findNext uniqueId options currentId =
    case options of
        [] ->
            Nothing

        first :: rest ->
            if uniqueId first == currentId then
                List.head rest
                    |> Maybe.map Next

            else
                Just (findNextHelp first uniqueId rest currentId)


findNextHelp : a -> (a -> String) -> List a -> String -> Next a
findNextHelp first uniqueId options currentId =
    case options of
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
range uniqueId options end start =
    case options of
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
rangeHelp uniqueId collected end options =
    case options of
        [] ->
            []

        a :: rest ->
            if uniqueId a == end then
                a :: collected

            else
                rangeHelp uniqueId (a :: collected) end rest
