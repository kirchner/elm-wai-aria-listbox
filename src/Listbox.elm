module Listbox
    exposing
        ( Behaviour
        , Event
        , HtmlAttributes
        , HtmlDetails
        , Ids
        , Listbox
        , Msg
        , TypeAhead
        , UpdateConfig
        , ViewConfig
        , Views
        , focused
        , noTypeAhead
        , onAllEntriesSelect
        , onAllEntriesUnselect
        , onEntriesSelect
        , onEntrySelect
        , onEntryUnselect
        , onEscapeDown
        , onListboxBlur
        , simpleTypeAhead
        , subscriptions
        , typeAhead
        , unfocused
        , update
        , updateConfig
        , view
        , viewConfig
        , viewLazy
        )

{-|

@docs Listbox, unfocused, view, Ids, update, Msg, subscriptions

@docs focused

@docs viewLazy


# Events

@docs Event

@docs onEntrySelect, onEntriesSelect, onAllEntriesSelect

@docs onEntryUnselect, onAllEntriesUnselect

@docs onListboxBlur, onEscapeDown


# Configuration

@docs Config, Behaviour, View

@docs HtmlAttributes, HtmlDetails


## Type ahead

@docs TypeAhead, noTypeAhead, simpleTypeAhead, typeAhead

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

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Internal.Entries
    exposing
        ( Next(..)
        , Previous(..)
        , find
        , findNext
        , findPrevious
        , findWith
        , indexOf
        , range
        )
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Set
import Task
import Time


{-| TODO
-}
type Listbox
    = Unfocused UnfocusedData
    | Focused FocusedData
    | Empty


type alias UnfocusedData =
    { preventScroll : Bool

    -- FOCUS
    , maybeKeyboardFocus : Maybe String
    , maybeMouseFocus : Maybe String
    , maybeLastSelectedEntry : Maybe String

    -- DOM
    , ulScrollTop : Float
    , ulClientHeight : Float
    }


type alias FocusedData =
    { query : Query

    -- FOCUS
    , keyboardFocus : String
    , maybeMouseFocus : Maybe String
    , maybeLastSelectedEntry : Maybe String

    -- DOM
    , ulScrollTop : Float
    , ulClientHeight : Float
    }


type Query
    = NoQuery
    | Query Int Time.Posix String


{-| TODO
-}
unfocused : Listbox
unfocused =
    Unfocused
        { preventScroll = False
        , maybeKeyboardFocus = Nothing
        , maybeMouseFocus = Nothing
        , maybeLastSelectedEntry = Nothing
        , ulScrollTop = 0
        , ulClientHeight = 1000
        }


{-| TODO
-}
focused : UpdateConfig a -> String -> a -> ( Listbox, Cmd (Msg a) )
focused (UpdateConfig uniqueId _) id focusedEntry =
    ( Focused
        { query = NoQuery
        , keyboardFocus = uniqueId focusedEntry
        , maybeMouseFocus = Nothing
        , maybeLastSelectedEntry = Nothing
        , ulScrollTop = 0
        , ulClientHeight = 1000
        }
    , Cmd.batch
        [ Browser.scrollIntoView (printEntryId id (uniqueId focusedEntry))
            |> Task.attempt (\_ -> NoOp)
        , focusList id
        ]
    )



---- VIEW CONFIG


{-| TODO
-}
type ViewConfig a
    = ViewConfig (a -> String) (Views a)


{-| TODO
-}
viewConfig : (a -> String) -> Views a -> ViewConfig a
viewConfig =
    ViewConfig


{-| TODO
-}
type alias Views a =
    { ul : HtmlAttributes
    , li :
        { selected : Bool
        , keyboardFocused : Bool
        , mouseFocused : Bool
        , maybeQuery : Maybe String
        }
        -> a
        -> HtmlDetails
    , empty : Html Never
    }


{-| TODO
-}
type alias HtmlAttributes =
    List (Html.Attribute Never)


{-| TODO
-}
type alias HtmlDetails =
    { attributes : List (Html.Attribute Never)
    , children : List (Html Never)
    }



---- UPDATE CONFIG


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
    { jumpAtEnds : Bool
    , separateFocus : Bool
    , selectionFollowsFocus : Bool
    , handleHomeAndEnd : Bool
    , typeAhead : TypeAhead a
    }


{-| TODO
-}
type TypeAhead a
    = NoTypeAhead
    | TypeAhead Int (String -> a -> Bool)


{-| TODO
-}
noTypeAhead : TypeAhead a
noTypeAhead =
    NoTypeAhead


{-| TODO
-}
simpleTypeAhead : Int -> (a -> String) -> TypeAhead a
simpleTypeAhead timeout entryToString =
    TypeAhead timeout <|
        \query entry ->
            String.toLower (entryToString entry)
                |> String.startsWith (String.toLower query)


{-| TODO
-}
typeAhead : Int -> (String -> a -> Bool) -> TypeAhead a
typeAhead =
    TypeAhead



---- VIEW


{-| TODO
-}
type alias Ids =
    { id : String
    , labelledBy : String
    }


{-| TODO
-}
view : ViewConfig a -> Ids -> Listbox -> List a -> List a -> Html (Msg a)
view (ViewConfig uniqueId views) ids listbox allEntries selection =
    let
        renderedEntries =
            { spaceAboveFirst = 0
            , droppedAboveFirst = 0
            , spaceAboveSecond = 0
            , droppedAboveSecond = 0
            , spaceBelowFirst = 0
            , droppedBelowFirst = 0
            , spaceBelowSecond = 0
            , droppedBelowSecond = 0
            , entriesAbove = []
            , visibleEntries = allEntries
            , entriesBelow = []
            }
    in
    viewHelp renderedEntries uniqueId views ids listbox allEntries selection


{-| TODO
-}
viewLazy : (a -> Float) -> ViewConfig a -> Ids -> Listbox -> List a -> List a -> Html (Msg a)
viewLazy entryHeight (ViewConfig uniqueId views) ids listbox allEntries selection =
    let
        renderedEntries =
            case listbox of
                Unfocused data ->
                    let
                        maybeFocusIndex =
                            case data.maybeKeyboardFocus of
                                Nothing ->
                                    Nothing

                                Just keyboardFocus ->
                                    find uniqueId keyboardFocus allEntries
                                        |> Maybe.map Tuple.first
                    in
                    computeRenderedEntries
                        entryHeight
                        data.ulScrollTop
                        data.ulClientHeight
                        maybeFocusIndex
                        allEntries

                Focused data ->
                    let
                        maybeFocusIndex =
                            find uniqueId data.keyboardFocus allEntries
                                |> Maybe.map Tuple.first
                    in
                    computeRenderedEntries
                        entryHeight
                        data.ulScrollTop
                        data.ulClientHeight
                        maybeFocusIndex
                        allEntries

                Empty ->
                    { spaceAboveFirst = 0
                    , droppedAboveFirst = 0
                    , spaceAboveSecond = 0
                    , droppedAboveSecond = 0
                    , spaceBelowFirst = 0
                    , droppedBelowFirst = 0
                    , spaceBelowSecond = 0
                    , droppedBelowSecond = 0
                    , entriesAbove = []
                    , visibleEntries = allEntries
                    , entriesBelow = []
                    }
    in
    viewHelp renderedEntries uniqueId views ids listbox allEntries selection


viewHelp :
    RenderedEntries a
    -> (a -> String)
    -> Views a
    -> Ids
    -> Listbox
    -> List a
    -> List a
    -> Html (Msg a)
viewHelp renderedEntries uniqueId views ids listbox allEntries selection =
    case listbox of
        Unfocused data ->
            viewUnfocused uniqueId
                views
                ids
                data.maybeKeyboardFocus
                selection
                allEntries
                renderedEntries.visibleEntries
                (viewEntries
                    uniqueId
                    views
                    ids
                    data.maybeKeyboardFocus
                    data.maybeMouseFocus
                    selection
                    Nothing
                    renderedEntries
                )

        Focused data ->
            let
                maybeQuery =
                    case data.query of
                        NoQuery ->
                            Nothing

                        Query _ _ query ->
                            Just query
            in
            viewFocused
                uniqueId
                views
                ids
                data.keyboardFocus
                renderedEntries.visibleEntries
                allEntries
                (viewEntries
                    uniqueId
                    views
                    ids
                    (Just data.keyboardFocus)
                    data.maybeMouseFocus
                    selection
                    maybeQuery
                    renderedEntries
                )

        Empty ->
            Html.map (\_ -> NoOp) views.empty


viewUnfocused :
    (a -> String)
    -> Views a
    -> Ids
    -> Maybe String
    -> List a
    -> List a
    -> List a
    -> List (Html (Msg a))
    -> Html (Msg a)
viewUnfocused uniqueId views ids maybeKeyboardFocus selection allEntries visibleEntries =
    Html.ul
        ([ Attributes.id (printListId ids.id)
         , Attributes.attribute "role" "listbox"
         , Attributes.attribute "aria-labelledby" ids.labelledBy
         , Attributes.tabindex 0
         , Events.onMouseDown ListMouseDown
         , Events.on "scroll" <|
            Decode.map2 ListScrolled
                (Decode.at [ "target", "scrollTop" ] Decode.float)
                (Decode.at [ "target", "clientHeight" ] Decode.float)
         , Events.on "focus"
            (Decode.oneOf
                [ case selection of
                    [] ->
                        case maybeKeyboardFocus of
                            Nothing ->
                                Decode.succeed Nothing

                            Just keyboardFocus ->
                                visibleEntries
                                    |> indexOf uniqueId keyboardFocus
                                    |> Maybe.map
                                        (\index ->
                                            Decode.map Just (scrollDataDecoder (index + 2))
                                        )
                                    |> Maybe.withDefault (Decode.succeed Nothing)

                    firstSelection :: _ ->
                        visibleEntries
                            |> indexOf uniqueId (uniqueId firstSelection)
                            |> Maybe.map (\index -> Decode.map Just (scrollDataDecoder (index + 2)))
                            |> Maybe.withDefault (Decode.succeed Nothing)
                , Decode.succeed Nothing
                ]
                |> Decode.map (ListFocused ids.id)
            )
         ]
            |> setAriaActivedescendant ids.id uniqueId maybeKeyboardFocus allEntries
            |> appendAttributes views.ul
        )


viewFocused :
    (a -> String)
    -> Views a
    -> Ids
    -> String
    -> List a
    -> List a
    -> List (Html (Msg a))
    -> Html (Msg a)
viewFocused uniqueId views ids keyboardFocus visibleEntries allEntries =
    Html.ul
        ([ Attributes.id (printListId ids.id)
         , Attributes.attribute "role" "listbox"
         , Attributes.attribute "aria-labelledby" ids.labelledBy
         , Attributes.tabindex 0
         , Events.preventDefaultOn "keydown"
            (keyInfoDecoder
                |> Decode.andThen
                    (listKeydown uniqueId ids.id keyboardFocus visibleEntries)
                |> preventDefault
            )
         , Events.on "scroll" <|
            Decode.map2 ListScrolled
                (Decode.at [ "target", "scrollTop" ] Decode.float)
                (Decode.at [ "target", "clientHeight" ] Decode.float)
         , Events.on "blur" (Decode.succeed ListBlured)
         ]
            |> setAriaActivedescendant ids.id uniqueId (Just keyboardFocus) allEntries
            |> appendAttributes views.ul
        )


type alias KeyInfo =
    { code : String
    , shiftDown : Bool
    , controlDown : Bool
    }


keyInfoDecoder : Decoder KeyInfo
keyInfoDecoder =
    Decode.succeed KeyInfo
        |> Decode.required "key" Decode.string
        |> Decode.required "shiftKey" Decode.bool
        |> Decode.required "ctrlKey" Decode.bool


listKeydown : (a -> String) -> String -> String -> List a -> KeyInfo -> Decoder (Msg a)
listKeydown uniqueId id keyboardFocus visibleEntries { code, shiftDown, controlDown } =
    case code of
        "ArrowUp" ->
            Decode.oneOf
                [ visibleEntries
                    |> indexOf uniqueId keyboardFocus
                    |> Maybe.map (\index -> Decode.map Just (scrollDataDecoder (index + 1)))
                    |> Maybe.withDefault (Decode.succeed Nothing)
                , Decode.succeed Nothing
                ]
                |> Decode.map (ListArrowUpPressed id shiftDown)

        "ArrowDown" ->
            Decode.oneOf
                [ visibleEntries
                    |> indexOf uniqueId keyboardFocus
                    |> Maybe.map (\index -> Decode.map Just (scrollDataDecoder (index + 3)))
                    |> Maybe.withDefault (Decode.succeed Nothing)
                , Decode.succeed Nothing
                ]
                |> Decode.map (ListArrowDownPressed id shiftDown)

        "Enter" ->
            Decode.succeed (ListEnterPressed id)

        "Escape" ->
            Decode.succeed ListEscapePressed

        " " ->
            if shiftDown then
                Decode.succeed (ListShiftSpacePressed id)
            else
                Decode.succeed (ListSpacePressed id)

        "Home" ->
            if shiftDown && controlDown then
                Decode.succeed (ListControlShiftHomePressed id)
            else
                Decode.succeed (ListHomePressed id)

        "End" ->
            if shiftDown && controlDown then
                Decode.succeed (ListControlShiftEndPressed id)
            else
                Decode.succeed (ListEndPressed id)

        "a" ->
            if controlDown then
                Decode.succeed ListControlAPressed
            else if String.length code == 1 then
                Decode.succeed (ListKeyPressed id code)
            else
                Decode.fail "not handling that key here"

        _ ->
            if String.length code == 1 then
                Decode.succeed (ListKeyPressed id code)
            else
                Decode.fail "not handling that key here"


scrollDataDecoder : Int -> Decoder ScrollData
scrollDataDecoder index =
    Decode.succeed ScrollData
        |> Decode.requiredAt
            [ "target", "scrollTop" ]
            Decode.float
        |> Decode.requiredAt
            [ "target", "clientHeight" ]
            Decode.float
        |> Decode.requiredAt
            [ "target", "childNodes", String.fromInt index, "offsetTop" ]
            Decode.float
        |> Decode.requiredAt
            [ "target", "childNodes", String.fromInt index, "offsetHeight" ]
            Decode.float


viewEntries :
    (a -> String)
    -> Views a
    -> Ids
    -> Maybe String
    -> Maybe String
    -> List a
    -> Maybe String
    -> RenderedEntries a
    -> List (Html (Msg a))
viewEntries uniqueId views ids maybeKeyboardFocus maybeMouseFocus selection maybeQuery renderedEntries =
    let
        entryConfig =
            { id = ids.id
            , li = views.li
            , uniqueId = uniqueId
            }

        viewEntryWrapper a =
            viewEntry entryConfig
                maybeQuery
                (List.member a selection)
                (maybeKeyboardFocus == Just (uniqueId a))
                (maybeMouseFocus == Just (uniqueId a))
                a
    in
    List.concat
        [ spacer renderedEntries.spaceAboveFirst
        , renderedEntries.entriesAbove
            |> List.map viewEntryWrapper
        , spacer renderedEntries.spaceAboveSecond
        , renderedEntries.visibleEntries
            |> List.map viewEntryWrapper
        , spacer renderedEntries.spaceBelowFirst
        , renderedEntries.entriesBelow
            |> List.map viewEntryWrapper
        , spacer renderedEntries.spaceBelowSecond
        ]


viewEntry :
    { id : String
    , li :
        { selected : Bool
        , keyboardFocused : Bool
        , mouseFocused : Bool
        , maybeQuery : Maybe String
        }
        -> a
        -> HtmlDetails
    , uniqueId : a -> String
    }
    -> Maybe String
    -> Bool
    -> Bool
    -> Bool
    -> a
    -> Html (Msg a)
viewEntry config maybeQuery selected keyboardFocused mouseFocused a =
    let
        { attributes, children } =
            config.li
                { selected = selected
                , keyboardFocused = keyboardFocused
                , mouseFocused = mouseFocused
                , maybeQuery = maybeQuery
                }
                a
    in
    Html.li
        ([ Events.onMouseEnter (EntryMouseEntered (config.uniqueId a))
         , Events.onMouseLeave EntryMouseLeft
         , Events.onClick (EntryClicked config.id a)
         , Attributes.id (printEntryId config.id (config.uniqueId a))
         , Attributes.attribute "role" "option"
         ]
            |> appendAttributes attributes
        )
        (children
            |> List.map (Html.map (\_ -> NoOp))
        )


spacer : Float -> List (Html msg)
spacer height =
    [ Html.li
        (if height == 0 then
            [ Attributes.style "display" "none" ]
         else
            [ Attributes.style "height" (String.fromFloat height ++ "px") ]
        )
        []
    ]



-- VIEW HELPER


setAriaActivedescendant :
    String
    -> (a -> String)
    -> Maybe String
    -> List a
    -> List (Html.Attribute msg)
    -> List (Html.Attribute msg)
setAriaActivedescendant id uniqueId maybeKeyboardFocus entries attrs =
    case maybeKeyboardFocus of
        Nothing ->
            attrs

        Just keyboardFocus ->
            entries
                |> find uniqueId keyboardFocus
                |> Maybe.map
                    (\( _, focusedEntry ) ->
                        Attributes.attribute "aria-activedescendant"
                            (printEntryId id (uniqueId focusedEntry))
                            :: attrs
                    )
                |> Maybe.withDefault attrs



---- UPDATE


{-| TODO
-}
type Msg a
    = NoOp
      -- LIST
    | ListMouseDown
    | ListFocused String (Maybe ScrollData)
    | ListBlured
    | ListScrolled Float Float
    | ListArrowUpPressed String Bool (Maybe ScrollData)
    | ListArrowDownPressed String Bool (Maybe ScrollData)
    | ListEnterPressed String
    | ListEscapePressed
    | ListSpacePressed String
    | ListShiftSpacePressed String
    | ListHomePressed String
    | ListControlShiftHomePressed String
    | ListEndPressed String
    | ListControlShiftEndPressed String
    | ListControlAPressed
      -- QUERY
    | ListKeyPressed String String
    | CurrentTimeReceived String String Time.Posix
    | Tick Time.Posix
      -- ENTRY
    | EntryMouseEntered String
    | EntryMouseLeft
    | EntryClicked String a


type alias ScrollData =
    { ulScrollTop : Float
    , ulClientHeight : Float
    , liOffsetTop : Float
    , liOffsetHeight : Float
    }


{-| TODO
-}
type Event a outMsg
    = OnEntrySelect (a -> outMsg)
    | OnEntriesSelect (List a -> outMsg)
    | OnEntryUnselect (a -> outMsg)
    | OnAllEntriesSelect outMsg
    | OnAllEntriesUnselect outMsg
    | OnListboxBlur outMsg
    | OnEscapeDown outMsg


{-| TODO
-}
onEntrySelect : (a -> outMsg) -> Event a outMsg
onEntrySelect =
    OnEntrySelect


{-| TODO
-}
onEntriesSelect : (List a -> outMsg) -> Event a outMsg
onEntriesSelect =
    OnEntriesSelect


{-| TODO
-}
onEntryUnselect : (a -> outMsg) -> Event a outMsg
onEntryUnselect =
    OnEntryUnselect


{-| TODO
-}
onAllEntriesSelect : outMsg -> Event a outMsg
onAllEntriesSelect =
    OnAllEntriesSelect


{-| TODO
-}
onAllEntriesUnselect : outMsg -> Event a outMsg
onAllEntriesUnselect =
    OnAllEntriesUnselect


{-| TODO
-}
onListboxBlur : outMsg -> Event a outMsg
onListboxBlur =
    OnListboxBlur


{-| TODO
-}
onEscapeDown : outMsg -> Event a outMsg
onEscapeDown =
    OnEscapeDown


sendEntrySelected : a -> List (Event a outMsg) -> Maybe outMsg
sendEntrySelected a events =
    case events of
        [] ->
            Nothing

        (OnEntrySelect entrySelected) :: _ ->
            Just (entrySelected a)

        _ :: rest ->
            sendEntrySelected a rest


sendEntriesSelected : List (Event a outMsg) -> List a -> Maybe outMsg
sendEntriesSelected events entries =
    case events of
        [] ->
            Nothing

        (OnEntriesSelect entriesSelected) :: _ ->
            Just (entriesSelected entries)

        _ :: rest ->
            sendEntriesSelected rest entries


sendEntryUnselected : a -> List (Event a outMsg) -> Maybe outMsg
sendEntryUnselected a events =
    case events of
        [] ->
            Nothing

        (OnEntryUnselect entryUnselected) :: _ ->
            Just (entryUnselected a)

        _ :: rest ->
            sendEntryUnselected a rest


sendAllEntriesSelected : List (Event a outMsg) -> Maybe outMsg
sendAllEntriesSelected events =
    case events of
        [] ->
            Nothing

        (OnAllEntriesSelect allEntriesSelected) :: _ ->
            Just allEntriesSelected

        _ :: rest ->
            sendAllEntriesSelected rest


sendAllEntriesUnselected : List (Event a outMsg) -> Maybe outMsg
sendAllEntriesUnselected events =
    case events of
        [] ->
            Nothing

        (OnAllEntriesUnselect allEntriesUnselected) :: _ ->
            Just allEntriesUnselected

        _ :: rest ->
            sendAllEntriesUnselected rest


sendListboxBlured : List (Event a outMsg) -> Maybe outMsg
sendListboxBlured events =
    case events of
        [] ->
            Nothing

        (OnListboxBlur listboxBlured) :: _ ->
            Just listboxBlured

        _ :: rest ->
            sendListboxBlured rest


sendEscapeDown : List (Event a outMsg) -> Maybe outMsg
sendEscapeDown events =
    case events of
        [] ->
            Nothing

        (OnEscapeDown escapeDowned) :: _ ->
            Just escapeDowned

        _ :: rest ->
            sendEscapeDown rest


{-| TODO
-}
update :
    UpdateConfig a
    -> List (Event a outMsg)
    -> Listbox
    -> List a
    -> List a
    -> Msg a
    -> ( Listbox, Cmd (Msg a), Maybe outMsg )
update config events listbox allEntries selection msg =
    case listbox of
        Unfocused unfocusedData ->
            updateUnfocused config events allEntries selection unfocusedData msg

        Focused focusedData ->
            updateFocused config events allEntries selection focusedData msg

        Empty ->
            ( listbox, Cmd.none, Nothing )


updateUnfocused :
    UpdateConfig a
    -> List (Event a outMsg)
    -> List a
    -> List a
    -> UnfocusedData
    -> Msg a
    -> ( Listbox, Cmd (Msg a), Maybe outMsg )
updateUnfocused (UpdateConfig uniqueId behaviour) events allEntries selection data msg =
    case msg of
        -- LIST
        ListMouseDown ->
            ( Unfocused { data | preventScroll = True }
            , Cmd.none
            , Nothing
            )

        ListFocused id maybeScrollData ->
            let
                maybeNewFocus =
                    data.maybeLastSelectedEntry
                        |> or (Maybe.map uniqueId (List.head selection))
                        |> or data.maybeKeyboardFocus
                        |> or (Maybe.map uniqueId (List.head allEntries))

                or fallback default =
                    case default of
                        Nothing ->
                            fallback

                        Just _ ->
                            default
            in
            case maybeNewFocus of
                Nothing ->
                    ( Empty
                    , Cmd.none
                    , Nothing
                    )

                Just newFocus ->
                    ( unfocusedToFocused behaviour newFocus data
                    , if data.preventScroll then
                        Cmd.none
                      else
                        adjustScrollTop id newFocus maybeScrollData
                    , if behaviour.selectionFollowsFocus then
                        case find uniqueId newFocus allEntries of
                            Nothing ->
                                Nothing

                            Just ( _, a ) ->
                                sendEntrySelected a events
                      else
                        Nothing
                    )

        ListScrolled ulScrollTop ulClientHeight ->
            ( Unfocused
                { data
                    | ulScrollTop = ulScrollTop
                    , ulClientHeight = ulClientHeight
                }
            , Cmd.none
            , Nothing
            )

        -- ENTRY
        EntryMouseEntered newFocus ->
            ( Unfocused
                { data
                    | maybeKeyboardFocus =
                        if behaviour.separateFocus then
                            data.maybeKeyboardFocus
                        else
                            Just newFocus
                    , maybeMouseFocus = Just newFocus
                }
            , Cmd.none
            , Nothing
            )

        EntryMouseLeft ->
            ( Unfocused
                { data
                    | maybeMouseFocus =
                        if behaviour.separateFocus then
                            Nothing
                        else
                            data.maybeMouseFocus
                }
            , Cmd.none
            , Nothing
            )

        EntryClicked id a ->
            ( Focused
                { query = NoQuery

                -- FOCUS
                , keyboardFocus = uniqueId a
                , maybeMouseFocus = Just (uniqueId a)
                , maybeLastSelectedEntry = Just (uniqueId a)

                -- DOM
                , ulScrollTop = 0
                , ulClientHeight = 1000
                }
            , focusList id
            , toggleSelectState events selection a
            )

        _ ->
            ( Unfocused data, Cmd.none, Nothing )


updateFocused :
    UpdateConfig a
    -> List (Event a outMsg)
    -> List a
    -> List a
    -> FocusedData
    -> Msg a
    -> ( Listbox, Cmd (Msg a), Maybe outMsg )
updateFocused (UpdateConfig uniqueId behaviour) events allEntries selection data msg =
    case msg of
        -- LIST
        ListBlured ->
            ( Unfocused
                { preventScroll = False
                , maybeKeyboardFocus = Just data.keyboardFocus
                , maybeMouseFocus = data.maybeMouseFocus
                , maybeLastSelectedEntry = data.maybeLastSelectedEntry
                , ulScrollTop = data.ulScrollTop
                , ulClientHeight = data.ulClientHeight
                }
            , Cmd.none
            , sendListboxBlured events
            )

        ListScrolled ulScrollTop ulClientHeight ->
            ( Focused
                { data
                    | ulScrollTop = ulScrollTop
                    , ulClientHeight = ulClientHeight
                }
            , Cmd.none
            , Nothing
            )

        ListArrowUpPressed id shiftDown maybeScrollData ->
            case findPrevious uniqueId data.keyboardFocus allEntries of
                Just (Last lastEntry) ->
                    if behaviour.jumpAtEnds then
                        data
                            |> updateFocus behaviour uniqueId events selection shiftDown lastEntry
                            |> andDo (scrollListToBottom id)
                    else
                        ( Focused { data | query = NoQuery }
                        , Cmd.none
                        , Nothing
                        )

                Just (Previous newIndex newEntry) ->
                    data
                        |> updateFocus behaviour uniqueId events selection shiftDown newEntry
                        |> andDo (adjustScrollTop id (uniqueId newEntry) maybeScrollData)

                Nothing ->
                    ( Focused data
                    , Cmd.none
                    , Nothing
                    )

        ListArrowDownPressed id shiftDown maybeScrollData ->
            case findNext uniqueId data.keyboardFocus allEntries of
                Just (First firstEntry) ->
                    if behaviour.jumpAtEnds then
                        data
                            |> updateFocus behaviour uniqueId events selection shiftDown firstEntry
                            |> andDo (scrollListToTop id)
                    else
                        ( Focused { data | query = NoQuery }
                        , Cmd.none
                        , Nothing
                        )

                Just (Next newIndex newEntry) ->
                    data
                        |> updateFocus behaviour uniqueId events selection shiftDown newEntry
                        |> andDo (adjustScrollTop id (uniqueId newEntry) maybeScrollData)

                Nothing ->
                    ( Focused data
                    , Cmd.none
                    , Nothing
                    )

        ListEnterPressed id ->
            case find uniqueId data.keyboardFocus allEntries of
                Nothing ->
                    ( Focused data
                    , Cmd.none
                    , Nothing
                    )

                Just ( _, a ) ->
                    if List.member a selection then
                        ( Focused { data | maybeLastSelectedEntry = Nothing }
                        , Cmd.none
                        , sendEntryUnselected a events
                        )
                    else
                        ( Focused { data | maybeLastSelectedEntry = Just (uniqueId a) }
                        , Cmd.none
                        , sendEntrySelected a events
                        )

        ListEscapePressed ->
            ( Focused data
            , Cmd.none
            , sendEscapeDown events
            )

        ListSpacePressed id ->
            case find uniqueId data.keyboardFocus allEntries of
                Nothing ->
                    ( Focused data
                    , Cmd.none
                    , Nothing
                    )

                Just ( _, a ) ->
                    if List.member a selection then
                        ( Focused { data | maybeLastSelectedEntry = Nothing }
                        , Cmd.none
                        , sendEntryUnselected a events
                        )
                    else
                        ( Focused { data | maybeLastSelectedEntry = Just (uniqueId a) }
                        , Cmd.none
                        , sendEntrySelected a events
                        )

        ListShiftSpacePressed id ->
            case data.maybeLastSelectedEntry of
                Nothing ->
                    ( Focused data
                    , Cmd.none
                    , Nothing
                    )

                Just lastSelectedEntry ->
                    case range uniqueId data.keyboardFocus lastSelectedEntry allEntries of
                        [] ->
                            ( Focused data
                            , Cmd.none
                            , Nothing
                            )

                        selectedEntries ->
                            ( Focused { data | maybeLastSelectedEntry = Just data.keyboardFocus }
                            , Cmd.none
                            , sendEntriesSelected events selectedEntries
                            )

        ListHomePressed id ->
            case List.head allEntries of
                Nothing ->
                    ( Focused data
                    , Cmd.none
                    , Nothing
                    )

                Just firstEntry ->
                    data
                        |> updateFocus behaviour uniqueId events [] False firstEntry
                        |> andDo (scrollListToTop id)

        ListControlShiftHomePressed id ->
            case List.head allEntries of
                Nothing ->
                    ( Focused data
                    , Cmd.none
                    , Nothing
                    )

                Just firstEntry ->
                    let
                        newFocus =
                            uniqueId firstEntry
                    in
                    case range uniqueId newFocus data.keyboardFocus allEntries of
                        [] ->
                            ( Focused data
                            , Cmd.none
                            , Nothing
                            )

                        selectedEntries ->
                            ( Focused
                                { data
                                    | keyboardFocus = newFocus
                                    , maybeMouseFocus =
                                        if behaviour.separateFocus then
                                            data.maybeMouseFocus
                                        else
                                            Just newFocus
                                    , maybeLastSelectedEntry = Just newFocus
                                }
                            , scrollListToTop id
                            , sendEntriesSelected events selectedEntries
                            )

        ListEndPressed id ->
            case List.head (List.reverse allEntries) of
                Nothing ->
                    ( Focused data, Cmd.none, Nothing )

                Just lastEntry ->
                    data
                        |> updateFocus behaviour uniqueId events [] False lastEntry
                        |> andDo (scrollListToBottom id)

        ListControlShiftEndPressed id ->
            case List.head (List.reverse allEntries) of
                Nothing ->
                    ( Focused data
                    , Cmd.none
                    , Nothing
                    )

                Just lastEntry ->
                    let
                        newFocus =
                            uniqueId lastEntry
                    in
                    case range uniqueId newFocus data.keyboardFocus allEntries of
                        [] ->
                            ( Focused data
                            , Cmd.none
                            , Nothing
                            )

                        selectedEntries ->
                            ( Focused
                                { data
                                    | keyboardFocus = newFocus
                                    , maybeMouseFocus =
                                        if behaviour.separateFocus then
                                            data.maybeMouseFocus
                                        else
                                            Just newFocus
                                    , maybeLastSelectedEntry = Just newFocus
                                }
                            , scrollListToBottom id
                            , sendEntriesSelected events selectedEntries
                            )

        ListControlAPressed ->
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
            ( Focused data
            , Cmd.none
            , if Set.isEmpty (Set.diff allEntriesSet selectionSet) then
                sendAllEntriesUnselected events
              else
                sendAllEntriesSelected events
            )

        -- QUERY
        ListKeyPressed id code ->
            case behaviour.typeAhead of
                NoTypeAhead ->
                    ( Focused data
                    , Cmd.none
                    , Nothing
                    )

                TypeAhead _ _ ->
                    ( Focused data
                    , Time.now
                        |> Task.perform
                            (CurrentTimeReceived id code)
                    , Nothing
                    )

        CurrentTimeReceived id code currentTime ->
            case behaviour.typeAhead of
                NoTypeAhead ->
                    ( Focused data
                    , Cmd.none
                    , Nothing
                    )

                TypeAhead timeout matchesQuery ->
                    let
                        ( newQuery, queryText ) =
                            case data.query of
                                NoQuery ->
                                    ( Query timeout currentTime code, code )

                                Query _ _ query ->
                                    ( Query timeout currentTime (query ++ code), query ++ code )

                        newKeyboardFocus =
                            findWith matchesQuery uniqueId data.keyboardFocus queryText allEntries
                    in
                    case newKeyboardFocus of
                        Nothing ->
                            ( Focused data, Cmd.none, Nothing )

                        Just newFocus ->
                            ( Focused
                                { data
                                    | query = newQuery
                                    , keyboardFocus = newFocus
                                    , maybeMouseFocus =
                                        if behaviour.separateFocus then
                                            data.maybeMouseFocus
                                        else
                                            Just newFocus
                                }
                            , Browser.scrollIntoView (printEntryId id newFocus)
                                |> Task.attempt (\_ -> NoOp)
                            , Nothing
                            )

        Tick currentTime ->
            ( case data.query of
                NoQuery ->
                    Focused data

                Query timeout time _ ->
                    if Time.posixToMillis currentTime - Time.posixToMillis time > timeout then
                        Focused { data | query = NoQuery }
                    else
                        Focused data
            , Cmd.none
            , Nothing
            )

        -- ENTRY
        EntryMouseEntered newFocus ->
            ( Focused
                { data
                    | keyboardFocus =
                        if behaviour.separateFocus then
                            data.keyboardFocus
                        else
                            newFocus
                    , maybeMouseFocus = Just newFocus
                }
            , Cmd.none
            , Nothing
            )

        EntryMouseLeft ->
            ( Focused
                { data
                    | maybeMouseFocus =
                        if behaviour.separateFocus then
                            Nothing
                        else
                            data.maybeMouseFocus
                }
            , Cmd.none
            , Nothing
            )

        EntryClicked id a ->
            let
                newFocus =
                    uniqueId a
            in
            ( Focused
                { data
                    | query = NoQuery
                    , keyboardFocus = newFocus
                    , maybeMouseFocus =
                        if behaviour.separateFocus then
                            data.maybeMouseFocus
                        else
                            Just newFocus
                }
            , Cmd.none
            , if List.member a selection then
                sendEntryUnselected a events
              else
                sendEntrySelected a events
            )

        _ ->
            ( Focused data, Cmd.none, Nothing )


andDo : Cmd msg -> ( a, b ) -> ( a, Cmd msg, b )
andDo cmd ( a, b ) =
    ( a, cmd, b )


updateFocus :
    { b
        | separateFocus : Bool
        , selectionFollowsFocus : Bool
    }
    -> (a -> String)
    -> List (Event a outMsg)
    -> List a
    -> Bool
    -> a
    -> FocusedData
    -> ( Listbox, Maybe outMsg )
updateFocus behaviour uniqueId events selection shiftDown newEntry data =
    let
        newFocus =
            uniqueId newEntry
    in
    if behaviour.selectionFollowsFocus then
        ( Focused
            { data
                | query = NoQuery
                , keyboardFocus = newFocus
                , maybeMouseFocus =
                    if behaviour.separateFocus then
                        data.maybeMouseFocus
                    else
                        Just newFocus
                , maybeLastSelectedEntry = Just newFocus
            }
        , sendEntrySelected newEntry events
        )
    else if shiftDown then
        if List.member newEntry selection then
            ( Focused
                { data
                    | query = NoQuery
                    , keyboardFocus = newFocus
                    , maybeMouseFocus =
                        if behaviour.separateFocus then
                            data.maybeMouseFocus
                        else
                            Just newFocus
                    , maybeLastSelectedEntry = Nothing
                }
            , sendEntryUnselected newEntry events
            )
        else
            ( Focused
                { data
                    | query = NoQuery
                    , keyboardFocus = newFocus
                    , maybeMouseFocus =
                        if behaviour.separateFocus then
                            data.maybeMouseFocus
                        else
                            Just newFocus
                    , maybeLastSelectedEntry = Just newFocus
                }
            , sendEntrySelected newEntry events
            )
    else
        ( Focused
            { data
                | query = NoQuery
                , keyboardFocus = newFocus
                , maybeMouseFocus =
                    if behaviour.separateFocus then
                        data.maybeMouseFocus
                    else
                        Just newFocus
            }
        , Nothing
        )


unfocusedToFocused : Behaviour a -> String -> UnfocusedData -> Listbox
unfocusedToFocused behaviour keyboardFocus data =
    Focused
        { query = NoQuery
        , keyboardFocus = keyboardFocus
        , maybeMouseFocus =
            if behaviour.separateFocus then
                Nothing
            else
                Just keyboardFocus
        , maybeLastSelectedEntry = data.maybeLastSelectedEntry
        , ulScrollTop = data.ulScrollTop
        , ulClientHeight = data.ulClientHeight
        }


toggleSelectState : List (Event a outMsg) -> List a -> a -> Maybe outMsg
toggleSelectState events selection a =
    if List.member a selection then
        sendEntryUnselected a events
    else
        sendEntrySelected a events



---- SUBSCRIPTIONS


{-| TODO
-}
subscriptions : Listbox -> Sub (Msg a)
subscriptions listbox =
    case listbox of
        Unfocused _ ->
            Sub.none

        Focused data ->
            case data.query of
                NoQuery ->
                    Sub.none

                Query timeout _ _ ->
                    Time.every (toFloat (timeout // 3)) Tick

        Empty ->
            Sub.none



-- CMDS


focusList : String -> Cmd (Msg a)
focusList id =
    Browser.focus (printListId id)
        |> Task.attempt (\_ -> NoOp)


scrollListToTop : String -> Cmd (Msg a)
scrollListToTop id =
    Browser.setScrollTop (printListId id) 0
        |> Task.attempt (\_ -> NoOp)


scrollListToBottom : String -> Cmd (Msg a)
scrollListToBottom id =
    Browser.setScrollBottom (printListId id) 0
        |> Task.attempt (\_ -> NoOp)


adjustScrollTop : String -> String -> Maybe ScrollData -> Cmd (Msg a)
adjustScrollTop id entryId maybeScrollData =
    case maybeScrollData of
        Nothing ->
            Browser.scrollIntoView (printEntryId id entryId)
                |> Task.attempt (\_ -> NoOp)

        Just ({ ulScrollTop, ulClientHeight, liOffsetTop, liOffsetHeight } as scrollData) ->
            if (liOffsetTop + liOffsetHeight) > (ulScrollTop + ulClientHeight) then
                if liOffsetTop <= ulScrollTop + ulClientHeight then
                    Browser.setScrollTop (printListId id)
                        (liOffsetTop + liOffsetHeight - ulClientHeight)
                        |> Task.attempt (\_ -> NoOp)
                else
                    centerScrollTop id scrollData
            else if liOffsetTop < ulScrollTop then
                if liOffsetTop + liOffsetHeight >= ulScrollTop then
                    Browser.setScrollTop (printListId id) liOffsetTop
                        |> Task.attempt (\_ -> NoOp)
                else
                    centerScrollTop id scrollData
            else
                Cmd.none


centerScrollTop : String -> ScrollData -> Cmd (Msg a)
centerScrollTop id { ulClientHeight, liOffsetTop, liOffsetHeight } =
    Browser.setScrollTop (printListId id)
        (liOffsetTop + liOffsetHeight / 2 - ulClientHeight / 2)
        |> Task.attempt (\_ -> NoOp)



---- IDS


printListId : String -> String
printListId id =
    id ++ "__element-list"


printEntryId : String -> String -> String
printEntryId id entryId =
    id ++ "__element--" ++ entryId



-- MISC


appendAttributes :
    List (Html.Attribute Never)
    -> List (Html.Attribute (Msg a))
    -> List (Html.Attribute (Msg a))
appendAttributes neverAttrs attrs =
    neverAttrs
        |> List.map (Attributes.map (\_ -> NoOp))
        |> List.append attrs


preventDefault : Decoder msg -> Decoder ( msg, Bool )
preventDefault decoder =
    decoder
        |> Decode.map (\msg -> ( msg, True ))


allowDefault : Decoder msg -> Decoder ( msg, Bool )
allowDefault decoder =
    decoder
        |> Decode.map (\msg -> ( msg, False ))



---- COMPUTE RENDERED ENTRIES


type alias RenderedEntries a =
    { spaceAboveFirst : Float
    , droppedAboveFirst : Int
    , spaceAboveSecond : Float
    , droppedAboveSecond : Int
    , spaceBelowFirst : Float
    , droppedBelowFirst : Int
    , spaceBelowSecond : Float
    , droppedBelowSecond : Int
    , entriesAbove : List a
    , visibleEntries : List a
    , entriesBelow : List a
    }


computeRenderedEntries : (a -> Float) -> Float -> Float -> Maybe Int -> List a -> RenderedEntries a
computeRenderedEntries entryHeight ulScrollTop ulClientHeight maybeFocusIndex entries =
    let
        initialRenderedEntries =
            { spaceAboveFirst = 0
            , droppedAboveFirst = 0
            , spaceAboveSecond = 0
            , droppedAboveSecond = 0
            , spaceBelowFirst = 0
            , droppedBelowFirst = 0
            , spaceBelowSecond = 0
            , droppedBelowSecond = 0
            , entriesAbove = []
            , visibleEntries = []
            , entriesBelow = []
            }

        withoutIndex entry currentHeight data =
            let
                height =
                    entryHeight entry
            in
            if currentHeight < ulScrollTop - 200 then
                -- entry is above the rendered range
                { data
                    | spaceAboveFirst = data.spaceAboveFirst + height
                    , droppedAboveFirst = data.droppedAboveFirst + 1
                }
            else if currentHeight >= (ulScrollTop + ulClientHeight + 200) then
                { data
                    | spaceBelowFirst = data.spaceBelowFirst + height
                    , droppedBelowFirst = data.droppedBelowFirst + 1
                }
            else
                -- entry is within the rendered range
                { data | visibleEntries = entry :: data.visibleEntries }

        withIndex index currentIndex entry currentHeight data =
            let
                height =
                    entryHeight entry
            in
            if currentHeight < ulScrollTop - 200 then
                -- entry is above the rendered range
                if currentIndex < index - 1 then
                    -- entry is before focused entry
                    { data
                        | spaceAboveFirst = data.spaceAboveFirst + height
                        , droppedAboveFirst = data.droppedAboveFirst + 1
                    }
                else if currentIndex > index + 1 then
                    -- entry is after focused entry
                    { data
                        | spaceAboveSecond = data.spaceAboveSecond + height
                        , droppedAboveSecond = data.droppedAboveSecond + 1
                    }
                else
                    -- entry is focused or next to focused entry
                    { data | entriesAbove = entry :: data.entriesAbove }
            else if currentHeight > (ulScrollTop + ulClientHeight + 200) then
                -- entry is below the rendered range
                if currentIndex < index - 1 then
                    -- entry is before focused entry
                    { data
                        | spaceBelowFirst = data.spaceBelowFirst + height
                        , droppedBelowFirst = data.droppedBelowFirst + 1
                    }
                else if currentIndex > index + 1 then
                    -- entry is after focused entry
                    { data
                        | spaceBelowSecond = data.spaceBelowSecond + height
                        , droppedBelowSecond = data.droppedBelowSecond + 1
                    }
                else
                    -- entry is focused or next to focused entry
                    { data | entriesBelow = entry :: data.entriesBelow }
            else
                -- entry is within the rendered range
                { data | visibleEntries = entry :: data.visibleEntries }

        reverseLists renderedEntries =
            { renderedEntries
                | entriesAbove = List.reverse renderedEntries.entriesAbove
                , visibleEntries = List.reverse renderedEntries.visibleEntries
                , entriesBelow = List.reverse renderedEntries.entriesBelow
            }
    in
    reverseLists <|
        case maybeFocusIndex of
            Nothing ->
                entries
                    |> List.foldl
                        (\entry ( currentHeight, data ) ->
                            ( currentHeight + entryHeight entry
                            , withoutIndex entry currentHeight data
                            )
                        )
                        ( 0, initialRenderedEntries )
                    |> Tuple.second

            Just index ->
                entries
                    |> List.foldl
                        (\entry ( ( currentIndex, currentHeight ), data ) ->
                            ( ( currentIndex + 1
                              , currentHeight + entryHeight entry
                              )
                            , withIndex index currentIndex entry currentHeight data
                            )
                        )
                        ( ( 0, 0 ), initialRenderedEntries )
                    |> Tuple.second
