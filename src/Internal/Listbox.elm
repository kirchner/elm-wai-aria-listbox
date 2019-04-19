module Internal.Listbox exposing
    ( Behaviour
    , DomFunctions
    , Effect(..)
    , Entry(..)
    , EntryDomData
    , Focus(..)
    , Instance
    , Listbox
    , Msg(..)
    , Query(..)
    , TypeAhead(..)
    , UpdateConfig
    , ViewConfig
    , Views
    , currentFocus
    , find
    , focusEntry
    , focusNextOrFirstEntry
    , focusPreviousOrFirstEntry
    , focusedEntry
    , focusedEntryId
    , hoveredEntry
    , init
    , preventDefaultOnKeyDown
    , printEntryId
    , printListId
    , scrollToFocus
    , subscriptions
    , update
    , view
    )

import Browser.Dom as Dom
import Dict
import Internal.KeyInfo as KeyInfo exposing (KeyInfo)
import Internal.Label exposing (Label(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import Set
import Task exposing (Task)
import Time exposing (Posix)



---- MODEL


type alias Listbox =
    { preventScroll : Bool
    , query : Query

    -- FOCUS
    , focus : Focus
    , hover : Maybe String
    , maybeLastSelectedEntry : Maybe String
    }


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
currentFocus focus =
    case focus of
        NoFocus ->
            Nothing

        Focus current ->
            Just current

        Pending { current } ->
            Just current


type Query
    = NoQuery
    | Query Int Time.Posix String



---- INIT


init : Listbox
init =
    { preventScroll = False
    , query = NoQuery
    , focus = NoFocus
    , hover = Nothing
    , maybeLastSelectedEntry = Nothing
    }



---- ENTRY


type Entry a divider
    = Option a
    | Divider divider



---- EXTERNAL STATE MANIPULATION


focusedEntry : UpdateConfig a -> List (Entry a divider) -> Listbox -> Maybe a
focusedEntry { uniqueId } allEntries { focus } =
    Maybe.andThen (find uniqueId allEntries) (currentFocus focus)


hoveredEntry : UpdateConfig a -> List (Entry a divider) -> Listbox -> Maybe a
hoveredEntry { uniqueId } allEntries { hover } =
    Maybe.andThen (find uniqueId allEntries) hover


focusEntry : UpdateConfig a -> a -> Listbox -> List a -> ( Listbox, List a )
focusEntry { uniqueId, behaviour } a listbox selection =
    ( { listbox
        | query = NoQuery
        , focus = Focus (uniqueId a)
      }
    , if behaviour.selectionFollowsFocus then
        [ a ]

      else
        selection
    )


focusNextOrFirstEntry :
    UpdateConfig a
    -> List (Entry a divider)
    -> Listbox
    -> List a
    -> ( Listbox, List a )
focusNextOrFirstEntry config allEntries listbox selection =
    let
        { uniqueId, behaviour } =
            config

        maybeA =
            case currentFocus listbox.focus of
                Nothing ->
                    firstEntry allEntries

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
            ( listbox, selection )

        Just a ->
            let
                newListbox =
                    { listbox | focus = Focus (uniqueId a) }
            in
            if behaviour.selectionFollowsFocus then
                ( newListbox
                , [ a ]
                )

            else
                ( newListbox
                , selection
                )


focusPreviousOrFirstEntry :
    UpdateConfig a
    -> List (Entry a divider)
    -> Listbox
    -> List a
    -> ( Listbox, List a )
focusPreviousOrFirstEntry config allEntries listbox selection =
    let
        { uniqueId, behaviour } =
            config

        maybeA =
            case currentFocus listbox.focus of
                Nothing ->
                    firstEntry allEntries

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
            ( listbox, selection )

        Just a ->
            let
                newListbox =
                    { listbox | focus = Focus (uniqueId a) }
            in
            if behaviour.selectionFollowsFocus then
                ( newListbox
                , [ a ]
                )

            else
                ( newListbox
                , selection
                )


scrollToFocus : Behaviour a -> String -> Listbox -> Effect a
scrollToFocus behaviour id listbox =
    case listbox.focus of
        NoFocus ->
            CmdNone

        Focus current ->
            ScrollToOption behaviour id current Nothing

        Pending { current } ->
            ScrollToOption behaviour id current Nothing



---- VIEW CONFIG


type alias ViewConfig a divider attributeNever htmlNever =
    { uniqueId : a -> String
    , views : Views a divider attributeNever htmlNever
    }


type alias Views a divider attributeNever htmlNever =
    { ul : DomAttributes attributeNever
    , liOption :
        { selected : Bool
        , focused : Bool
        , hovered : Bool
        , maybeQuery : Maybe String
        }
        -> a
        -> DomDetails attributeNever htmlNever
    , liDivider : divider -> DomDetails attributeNever htmlNever
    , empty : htmlNever
    , focusable : Bool
    , markActiveDescendant : Bool
    }


type alias DomAttributes attributeNever =
    List attributeNever


type alias DomDetails attributeNever htmlNever =
    { attributes : List attributeNever
    , children : List htmlNever
    }


type alias DomFunctions attribute attributeNever html htmlNever msg =
    { ul : List attribute -> List html -> html
    , li : List attribute -> List html -> html
    , property : String -> Value -> attribute
    , attribute : String -> String -> attribute
    , on : String -> Decoder msg -> attribute
    , preventDefaultOn : String -> Decoder ( msg, Bool ) -> attribute
    , attributeMap : msg -> attributeNever -> attribute
    , htmlMap : msg -> htmlNever -> html
    }



---- UPDATE CONFIG


type alias UpdateConfig a =
    { uniqueId : a -> String
    , behaviour : Behaviour a
    }


type alias Behaviour a =
    { jumpAtEnds : Bool
    , separateFocus : Bool
    , selectionFollowsFocus : Bool
    , handleHomeAndEnd : Bool
    , typeAhead : TypeAhead a
    , minimalGap : Float
    , initialGap : Float
    }


type TypeAhead a
    = NoTypeAhead
    | TypeAhead Int (String -> a -> Bool)



---- VIEW


type alias Instance a msg =
    { id : String
    , label : Label
    , lift : Msg a -> msg
    }


view :
    Bool
    -> DomFunctions attribute attributeNever html htmlNever msg
    -> ViewConfig a divider attributeNever htmlNever
    -> Instance a msg
    -> List (Entry a divider)
    -> Listbox
    -> List a
    -> html
view multiSelectable dom config instance allEntries listbox selection =
    let
        { id, lift, label } =
            instance

        { uniqueId, views } =
            config

        viewEntryHelp entry =
            case entry of
                Option option ->
                    let
                        maybeHash =
                            Just (uniqueId option)

                        focused =
                            currentFocus listbox.focus == maybeHash

                        hovered =
                            listbox.hover == maybeHash

                        selected =
                            List.any ((==) option) selection
                    in
                    viewEntry dom
                        multiSelectable
                        focused
                        hovered
                        selected
                        config
                        instance
                        listbox.query
                        entry

                Divider _ ->
                    viewEntry dom
                        multiSelectable
                        False
                        False
                        False
                        config
                        instance
                        listbox.query
                        entry

        addAriaLabelledBy attrs =
            case label of
                LabelledBy labelledBy ->
                    dom.attribute "aria-labelledby" labelledBy :: attrs

                Label label_ ->
                    dom.attribute "aria-label" label_ :: attrs

                NoLabel ->
                    attrs

        addAriaActivedescendant focus attrs =
            if views.markActiveDescendant then
                let
                    setHelp a =
                        dom.attribute "aria-activedescendant"
                            (printEntryId id (uniqueId a))
                            :: attrs
                in
                focus
                    |> Maybe.andThen (find uniqueId allEntries)
                    |> Maybe.map setHelp
                    |> Maybe.withDefault attrs

            else
                attrs
    in
    if List.isEmpty allEntries then
        dom.htmlMap (lift NoOp) views.empty

    else
        dom.ul
            ([ dom.property "id" (Encode.string (printListId id))
             , dom.attribute "role" "listbox"
             , dom.attribute "aria-multiselectable" (stringFromBool multiSelectable)
             , dom.preventDefaultOn "keydown" <|
                Decode.andThen
                    (listKeyPress False id >> Decode.map (\msg -> ( lift msg, True )))
                    KeyInfo.decoder
             , dom.on "mousedown" (Decode.succeed (lift ListMouseDown))
             , dom.on "mouseup" (Decode.succeed (lift ListMouseUp))
             , dom.on "focus" (Decode.succeed (lift (ListFocused id)))
             , dom.on "blur" (Decode.succeed (lift ListBlured))
             ]
                |> addAriaLabelledBy
                |> addAriaActivedescendant (currentFocus listbox.focus)
                |> setTabindex dom.attribute views.focusable
                |> appendAttributes (dom.attributeMap (lift NoOp)) views.ul
            )
            (List.map viewEntryHelp allEntries)


viewEntry :
    DomFunctions attribute attributeNever html htmlNever msg
    -> Bool
    -> Bool
    -> Bool
    -> Bool
    -> ViewConfig a divider attributeNever htmlNever
    -> Instance a msg
    -> Query
    -> Entry a divider
    -> html
viewEntry dom multiSelectable focused hovered selected config instance query entry =
    let
        { uniqueId, views } =
            config

        { id, lift } =
            instance

        maybeQuery =
            case query of
                NoQuery ->
                    Nothing

                Query _ _ text ->
                    Just text
    in
    case entry of
        Option option ->
            let
                hash =
                    uniqueId option

                { attributes, children } =
                    views.liOption
                        { selected = selected
                        , focused = focused
                        , hovered = hovered
                        , maybeQuery = maybeQuery
                        }
                        option

                addWidgetSelected attrs =
                    if multiSelectable then
                        dom.attribute "aria-selected" (stringFromBool selected) :: attrs

                    else if selected then
                        dom.attribute "aria-selected" "true" :: attrs

                    else
                        attrs
            in
            dom.li
                ([ dom.property "id" (Encode.string (printEntryId id hash))
                 , dom.attribute "role" "option"
                 , dom.on "mouseenter" (Decode.succeed (lift (EntryMouseEntered hash)))
                 , dom.on "ouseleave" (Decode.succeed (lift EntryMouseLeft))
                 , dom.on "click" (Decode.succeed (lift (EntryClicked option)))
                 ]
                    |> addWidgetSelected
                    |> appendAttributes (dom.attributeMap (lift NoOp)) attributes
                )
                (List.map (dom.htmlMap (lift NoOp)) children)

        Divider d ->
            let
                { attributes, children } =
                    views.liDivider d
            in
            dom.li
                (appendAttributes (dom.attributeMap (lift NoOp)) attributes [])
                (List.map (dom.htmlMap (lift NoOp)) children)


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


preventDefaultOnKeyDown :
    (String -> Decoder ( msg, Bool ) -> attribute)
    -> Instance a msg
    -> Decoder ( msg, Bool )
    -> attribute
preventDefaultOnKeyDown preventDefaultOn { id, lift } keyDownDecoder =
    preventDefaultOn "keydown" <|
        Decode.oneOf
            [ keyDownDecoder
            , Decode.andThen
                (listKeyPress True id >> Decode.map (\msg -> ( lift msg, True )))
                KeyInfo.decoder
            ]


focusedEntryId :
    ViewConfig a divider attributeNever htmlNever
    -> Instance a msg
    -> List (Entry a divider)
    -> Listbox
    -> Maybe String
focusedEntryId { uniqueId } { id } entries { focus } =
    case focus of
        NoFocus ->
            Nothing

        Focus current ->
            find uniqueId entries current
                |> Maybe.map (printEntryId id << uniqueId)

        Pending { current } ->
            find uniqueId entries current
                |> Maybe.map (printEntryId id << uniqueId)



-- VIEW HELPER


setAriaActivedescendant :
    (String -> String -> attribute)
    -> String
    -> (a -> String)
    -> Maybe String
    -> List (Entry a divider)
    -> List attribute
    -> List attribute
setAriaActivedescendant attribute id uniqueId focus entries attrs =
    let
        setHelp a =
            attribute "aria-activedescendant" (printEntryId id (uniqueId a))
                :: attrs
    in
    focus
        |> Maybe.andThen (find uniqueId entries)
        |> Maybe.map setHelp
        |> Maybe.withDefault attrs


setTabindex : (String -> String -> attribute) -> Bool -> List attribute -> List attribute
setTabindex attribute focusable attrs =
    if focusable then
        attribute "tabindex" "0" :: attrs

    else
        attrs


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


type alias EntryDomData =
    { viewportList : Dom.Viewport
    , elementList : Dom.Element
    , elementLi : Dom.Element
    , elementPreviousLi : Dom.Element
    }


update :
    UpdateConfig a
    -> List (Entry a divider)
    -> Msg a
    -> Listbox
    -> List a
    -> ( Listbox, Effect a, List a )
update ({ uniqueId, behaviour } as config) allEntries msg listbox selection =
    let
        unchanged =
            ( listbox
            , CmdNone
            , selection
            )

        fromModel newListbox =
            ( newListbox
            , CmdNone
            , selection
            )

        withEffect effect ( newListbox, _, newSelection ) =
            ( newListbox, effect, newSelection )

        withSelection newSelection ( newListbox, effect, _ ) =
            ( newListbox, effect, newSelection )

        -- SELECTION
        select a listA ( newListbox, effect, newSelection ) =
            { newListbox | maybeLastSelectedEntry = Just (uniqueId a) }
                |> fromModel
                |> withSelection (List.uniqueBy uniqueId (a :: listA ++ newSelection))

        unselect a ( newListbox, effect, newSelection ) =
            newListbox
                |> fromModel
                |> withSelection (List.filter (\b -> a /= b) newSelection)

        toggle a ( newListbox, effect, newSelection ) =
            if List.member a newSelection then
                newListbox
                    |> fromModel
                    |> withSelection (List.filter (\b -> a /= b) newSelection)

            else
                { newListbox | maybeLastSelectedEntry = Just (uniqueId a) }
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
                        |> or (firstEntry allEntries)
            in
            case maybeA of
                Nothing ->
                    fromModel { listbox | query = NoQuery }

                Just a ->
                    let
                        hash =
                            uniqueId a

                        newListbox =
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
                            |> fromModel
                            |> withEffect
                                (GetViewportOfList (ViewportOfListReceived Up a) id)

                    else if behaviour.selectionFollowsFocus then
                        case find uniqueId allEntries current of
                            Nothing ->
                                fromModel { listbox | query = NoQuery }

                            Just currentA ->
                                if shiftDown then
                                    fromModel { listbox | query = NoQuery }
                                        |> toggle currentA

                                else
                                    fromModel { listbox | query = NoQuery }
                                        |> withSelection [ currentA ]

                    else
                        fromModel { listbox | query = NoQuery }

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
                            |> fromModel
                            |> withEffect
                                (GetViewportOfList (ViewportOfListReceived Down a) id)

                    else if behaviour.selectionFollowsFocus then
                        case find uniqueId allEntries current of
                            Nothing ->
                                fromModel { listbox | query = NoQuery }

                            Just currentA ->
                                if shiftDown then
                                    fromModel { listbox | query = NoQuery }
                                        |> toggle currentA

                                else
                                    fromModel { listbox | query = NoQuery }
                                        |> withSelection [ currentA ]

                    else
                        fromModel { listbox | query = NoQuery }

                Just (Next a) ->
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
                                    |> fromModel
                                    |> withSelection [ a ]
                                    |> withEffect (SetViewportOf id x y)

                            else if shiftDown then
                                newListbox
                                    |> fromModel
                                    |> toggle a
                                    |> withEffect (SetViewportOf id x y)

                            else
                                fromModel newListbox
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
                                    |> fromModel
                                    |> withSelection [ a ]
                                    |> withEffect effect

                            else if shiftDown then
                                newListbox
                                    |> fromModel
                                    |> toggle a
                                    |> withEffect effect

                            else
                                newListbox
                                    |> fromModel
                                    |> withEffect effect

        -- LIST
        ListMouseDown ->
            fromModel { listbox | preventScroll = True }

        ListMouseUp ->
            fromModel { listbox | preventScroll = False }

        ListFocused id ->
            if listbox.preventScroll then
                unchanged

            else
                initFocus id

        ListBlured ->
            fromModel
                { listbox
                    | query = NoQuery
                    , preventScroll = False
                }

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
            case focusedEntry config allEntries listbox of
                Nothing ->
                    unchanged

                Just a ->
                    unchanged
                        |> toggle a

        ListSpaceDown id ->
            case focusedEntry config allEntries listbox of
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
            case firstEntry allEntries of
                Nothing ->
                    unchanged

                Just a ->
                    { listbox
                        | query = NoQuery
                        , focus = Focus (uniqueId a)
                    }
                        |> fromModel
                        |> withEffect (ScrollListToTop id)

        ListControlShiftHomeDown id ->
            case Maybe.map uniqueId (firstEntry allEntries) of
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
                                |> fromModel
                                |> select a listA
                                |> withEffect (ScrollListToBottom id)

        ListControlADown ->
            let
                allEntriesSet =
                    allEntries
                        |> List.filterMap
                            (\e ->
                                case e of
                                    Divider _ ->
                                        Nothing

                                    Option a ->
                                        Just (uniqueId a)
                            )
                        |> Set.fromList

                allEntriesList =
                    allEntries
                        |> List.filterMap
                            (\e ->
                                case e of
                                    Divider _ ->
                                        Nothing

                                    Option a ->
                                        Just a
                            )

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
                    |> withSelection allEntriesList

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
                        fromModel { listbox | query = NoQuery }

                    else
                        unchanged

        -- ENTRY
        EntryMouseEntered newFocus ->
            fromModel
                { listbox
                    | focus =
                        if behaviour.separateFocus then
                            listbox.focus

                        else
                            Focus newFocus
                    , hover = Just newFocus
                }

        EntryMouseLeft ->
            fromModel
                { listbox
                    | hover =
                        if behaviour.separateFocus then
                            Nothing

                        else
                            listbox.hover
                }

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
                    |> fromModel
                    |> select a selection

            else
                { listbox
                    | query = NoQuery
                    , focus = Focus hash
                    , hover = Just hash
                }
                    |> fromModel
                    |> toggle a


focusPendingKeyboardFocus : Listbox -> Listbox
focusPendingKeyboardFocus listbox =
    case listbox.focus of
        NoFocus ->
            listbox

        Focus _ ->
            listbox

        Pending { pending } ->
            { listbox | focus = Focus pending }


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



-- HELPER


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



-- EFFECTS


focusList : String -> Effect a
focusList id =
    DomFocus (printListId id)



---- SUBSCRIPTIONS


subscriptions : Listbox -> Sub (Msg a)
subscriptions listbox =
    case listbox.query of
        NoQuery ->
            Sub.none

        Query timeout _ _ ->
            Time.every (toFloat (timeout // 3)) Tick



---- IDS


printListId : String -> String
printListId id =
    id


printEntryId : String -> String -> String
printEntryId id entryId =
    id ++ "__element--" ++ entryId



--- FIND


indexOf : (a -> String) -> List (Entry a divider) -> String -> Maybe Int
indexOf uniqueId entries selectedId =
    findHelp 0 uniqueId entries selectedId
        |> Maybe.map Tuple.first


find : (a -> String) -> List (Entry a divider) -> String -> Maybe a
find uniqueId entries selectedId =
    findHelp 0 uniqueId entries selectedId
        |> Maybe.map Tuple.second


findHelp :
    Int
    -> (a -> String)
    -> List (Entry a divider)
    -> String
    -> Maybe ( Int, a )
findHelp index uniqueId entries selectedId =
    case entries of
        [] ->
            Nothing

        (Divider _) :: rest ->
            findHelp (index + 1) uniqueId rest selectedId

        (Option entry) :: rest ->
            if uniqueId entry == selectedId then
                Just ( index, entry )

            else
                findHelp (index + 1) uniqueId rest selectedId


findWith :
    (String -> a -> Bool)
    -> (a -> String)
    -> String
    -> List (Entry a divider)
    -> String
    -> Maybe String
findWith matchesQuery uniqueId query entries id =
    case entries of
        [] ->
            Nothing

        (Divider _) :: rest ->
            findWith matchesQuery uniqueId query rest id

        (Option a) :: rest ->
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
    -> List (Entry a divider)
    -> Maybe String
proceedWith matchesQuery uniqueId id query entries =
    case entries of
        [] ->
            Just id

        (Divider _) :: rest ->
            proceedWith matchesQuery uniqueId id query rest

        (Option a) :: rest ->
            if matchesQuery query a then
                Just (uniqueId a)

            else
                proceedWith matchesQuery uniqueId id query rest


lastEntry : List (Entry a divider) -> Maybe a
lastEntry entries =
    firstEntry (List.reverse entries)


firstEntry : List (Entry a divider) -> Maybe a
firstEntry entries =
    case entries of
        [] ->
            Nothing

        (Divider _) :: rest ->
            firstEntry rest

        (Option a) :: _ ->
            Just a



---- PREVIOUS


type Previous a
    = Previous a
    | Last a


findPrevious :
    (a -> String)
    -> List (Entry a divider)
    -> String
    -> Maybe (Previous a)
findPrevious uniqueId entries currentId =
    case entries of
        [] ->
            Nothing

        (Divider _) :: rest ->
            findPrevious uniqueId rest currentId

        (Option first) :: rest ->
            if uniqueId first == currentId then
                entries
                    |> lastEntry
                    |> Maybe.map Last

            else
                findPreviousHelp first uniqueId rest currentId


findPreviousHelp :
    a
    -> (a -> String)
    -> List (Entry a divider)
    -> String
    -> Maybe (Previous a)
findPreviousHelp previous uniqueId entries currentId =
    case entries of
        [] ->
            Nothing

        (Divider _) :: rest ->
            findPreviousHelp previous uniqueId rest currentId

        (Option first) :: rest ->
            if uniqueId first == currentId then
                Just (Previous previous)

            else
                findPreviousHelp first uniqueId rest currentId



---- NEXT


type Next a
    = Next a
    | First a


findNext : (a -> String) -> List (Entry a divider) -> String -> Maybe (Next a)
findNext uniqueId entries currentId =
    case entries of
        [] ->
            Nothing

        (Divider _) :: rest ->
            findNext uniqueId rest currentId

        (Option first) :: rest ->
            if uniqueId first == currentId then
                firstEntry rest
                    |> Maybe.map Next

            else
                Just (findNextHelp first uniqueId rest currentId)


findNextHelp :
    a
    -> (a -> String)
    -> List (Entry a divider)
    -> String
    -> Next a
findNextHelp first uniqueId entries currentId =
    case entries of
        [] ->
            First first

        (Divider _) :: rest ->
            findNextHelp first uniqueId rest currentId

        (Option a) :: rest ->
            if uniqueId a == currentId then
                firstEntry rest
                    |> Maybe.map Next
                    |> Maybe.withDefault (First first)

            else
                findNextHelp first uniqueId rest currentId



---- RANGE


range : (a -> String) -> List (Entry a divider) -> String -> String -> List a
range uniqueId entries end start =
    case entries of
        [] ->
            []

        (Divider _) :: rest ->
            range uniqueId rest start end

        (Option a) :: rest ->
            if uniqueId a == start then
                rangeHelp uniqueId [ a ] end rest

            else if uniqueId a == end then
                List.reverse (rangeHelp uniqueId [ a ] start rest)

            else
                range uniqueId rest start end


rangeHelp : (a -> String) -> List a -> String -> List (Entry a divider) -> List a
rangeHelp uniqueId collected end entries =
    case entries of
        [] ->
            []

        (Divider _) :: rest ->
            rangeHelp uniqueId collected end rest

        (Option a) :: rest ->
            if uniqueId a == end then
                a :: collected

            else
                rangeHelp uniqueId (a :: collected) end rest



---- HELPER


stringFromBool : Bool -> String
stringFromBool bool =
    if bool then
        "true"

    else
        "false"
