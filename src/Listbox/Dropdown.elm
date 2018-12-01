module Listbox.Dropdown exposing
    ( Dropdown, init, view, Instance
    , update, Msg, subscriptions
    , UpdateConfig, updateConfig, Behaviour
    , ViewConfig, viewConfig, Views
    , customView, DomFunctions
    , CustomViewConfig(..), customViewConfig, CustomViews
    )

{-| This is a collapsible dropdown version of `Listbox`. The behaviour
is based on the [Collapsible Dropdown Listbox
Example](https://www.w3.org/TR/wai-aria-practices-1.1/examples/listbox/listbox-collapsible.html).

TODO: add ellie example

@docs Dropdown, init, view, Instance

@docs update, Msg, subscriptions


# Configuration

@docs UpdateConfig, updateConfig, Behaviour

@docs ViewConfig, viewConfig, Views


# Using different DOM libraries

@docs customView, DomFunctions

@docs CustomViewConfig, customViewConfig, CustomViews

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
import Accessibility.Widget as Widget
import Browser.Dom as Dom
import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Internal.KeyInfo as KeyInfo exposing (KeyInfo)
import Json.Decode as Decode exposing (Decoder)
import Listbox as Listbox
    exposing
        ( Entry
        , HtmlAttributes
        , HtmlDetails
        , Listbox
        , TypeAhead
        )
import Listbox.Unique as ListboxUnique
import Task


{-| Tracks the keyboard and mouse focus as well as the current query and
whether the dropdown is open or closed. The full list of entries and the
currently selected option(s) live in your own model.
-}
type Dropdown
    = Dropdown Data


type alias Data =
    { open : Bool
    , preventBlur : Bool
    , listbox : Listbox
    , pendingFocusListbox : Maybe String
    }


{-| An initial dropdown with no option focused, and which is closed.
-}
init : Dropdown
init =
    Dropdown
        { open = False
        , preventBlur = False
        , listbox = Listbox.init
        , pendingFocusListbox = Nothing
        }



---- VIEW CONFIG


{-| -}
type ViewConfig a divider
    = ViewConfig (a -> String) (Views a divider)


{-| Generate a `ViewConfig` by providing a hash function for the entries and
a `Views` record, which holds all the styling information. You usually do
**not** want to store this inside your model.
-}
viewConfig : (a -> String) -> Views a divider -> ViewConfig a divider
viewConfig =
    ViewConfig


{-| **Available view customizations**

This is the second argument to `viewConfig`. You can customize the styling with
the following fields:

  - **container**: A list of html attributes applied to the container div which
    holds the button and the listbox.

  - **button**: A function which returns `HtmlDetails` for the button which
    shows the current selection and toggles the visibility of the listbox. The
    function gets as arguments the current selection and whether the listbox is
    visible or not.

  - **ul**: A list of html attributes applied to the outer listbox.

  - **liOption**: A function which returns `HtmlDetails` for each option in
    your entries list. It gets the actual option value `a` and flags telling you
    if this option is currently `selected` or has focus (`keyboardFocus` and
    `mouseFocus`). If the user typed in a query, you get this via the
    `maybeQuery` field.

  - **liDivider**: This lets you style the divider list entries. It gets the
    actual `divider` entry and returns `HtmlDetails`.

The DOM structure of a dropdown will be something like this:

    listbox =
        Html.div
            [ ... ] -- container attributes
            [ Html.button
                [ ... ] -- button attributes
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

Provided you have specified some CSS classes, a view configuration could look
like this:

    views : Views String Never
    views =
        { container = [ Html.Attributes.class "dropdown__container" ]
        , button =
            \{ maybeSelection } ->
                { attributes =
                    [ Html.Attributes.class "dropdown__button" ]
                , children =
                    [ Html.text <|
                        Maybe.withDefault "Make a selection.."
                            maybeSelection
                    ]
                }
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
    , button :
        { maybeSelection : Maybe a
        , open : Bool
        }
        -> HtmlDetails
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



---- UPDATE CONFIG


{-| -}
type UpdateConfig a
    = UpdateConfig (a -> String) (Behaviour a)


{-| Generate an `UpdateConfig` by providing a hash function for the entries and
a `Behaviour` record.
-}
updateConfig : (a -> String) -> Behaviour a -> UpdateConfig a
updateConfig =
    UpdateConfig


{-| **Available behaviour customizations**

You can customize the behaviour of the dropdown with the following options:

  - **jumpAtEnds**: Should the keyboard focus jump to the other end of the list
    when pressing `ArrowUp` while focusing the first option (or `ArrowDown` while
    focusing the last).

  - **closeAfterMouseSelection**: Should the dropdown be hidden after the user
    selected an option with the mouse?

  - **separateFocus**: Whether the mouse focus and the keyboard focus can be
    different.

  - **selectionFollowsFocus**: Do we automatically select the entry gaining
    keyboard focus?

  - **handleHomeAndEnd**: Should we handle the `Home` and `End` keys (to jump
    to the top or bottom of the list)?

  - **typeAhead**: Make it possible to jump to options by typing in a query.
    Take a look at `TypeAhead` for more information.

  - **minimalGap**: If the distance (in px) of the option having the keyboard
    focus to the borders of the listbox scene is smaller then this value, the
    listbox will adjust its scroll position so that this distance is at least
    `initialGap`.

  - **initialGap**: The minimal distance (in px) of the option having the
    keyboard focus to the borders of the listbox scene after the scroll position
    has been adjusted.

A behaviour configuration could look something like this:

    behaviour : Behaviour String
    behaviour =
        { jumpAtEnds = True
        , closeAfterMouseSelection = False
        , separateFocus = True
        , selectionFollowsFocus = False
        , handleHomeAndEnd = True
        , typeAhead = simpleTypeAhead 300 identity
        , minimalGap = 30
        , initialGap = 200
        }

The dropdown will behave as explained in the [WAI-ARIA Authoring Practices
1.1](https://www.w3.org/TR/wai-aria-practices-1.1/examples/listbox/listbox-collapsible.html)
in the section _Keyboard Support_.

-}
type alias Behaviour a =
    { jumpAtEnds : Bool
    , closeAfterMouseSelection : Bool
    , separateFocus : Bool
    , selectionFollowsFocus : Bool
    , handleHomeAndEnd : Bool
    , typeAhead : TypeAhead a
    , minimalGap : Float
    , initialGap : Float
    }



---- VIEW


{-| To make a dropdown listbox unique in your application you have to provide
this information to the `view` function:

  - **id**: The unique id of the listbox.

  - **labelledBy**: The unique id of a label element describing the content of
    the listbox.

-}
type alias Instance msg a =
    { id : String
    , labelledBy : String
    , lift : Msg a -> msg
    }


{-| Take a list of all entries and a list of selected options and display it as
a dropdown. You have to provide a `ViewConfig` for the styling and an
`Instance` to uniquely identify this listbox. For example:

    view : Dropdown -> Maybe String -> Html Msg
    view dropdown selection =
        Html.div []
            [ Html.map DropdownMsg <|
                Listbox.Dropdown.view viewConfig
                    { id = "fruits-dropdown"
                    , labelledBy = "fruits"
                    }
                    fruits
                    dropdown
                    selection
            ]

    fruits : List (Entry String divider)
    fruits =
        List.map Listbox.option
            [ "Apple", "Banana", "Cherry" ]

    type Msg
        = DropdownMsg Listbox.Dropdown.Msg

-}
view :
    ViewConfig a divider
    -> Instance msg a
    -> List (Entry a divider)
    -> Dropdown
    -> Maybe a
    -> Html msg
view (ViewConfig uniqueId views) =
    customView htmlFunctions
        (CustomViewConfig uniqueId
            { container = views.container
            , button = views.button
            , ul = views.ul
            , liOption = views.liOption
            , liDivider = views.liDivider
            }
        )


htmlFunctions =
    { div =
        \attributes { button, ul } ->
            Html.div attributes
                [ button
                , ul
                ]
    , text = Html.text
    , button = Html.button
    , ul = Html.ul
    , li = Html.li
    , on = Events.on
    , preventDefaultOn = Events.preventDefaultOn
    , attribute = Attributes.attribute
    , style = Attributes.style
    , attributeMap = \noOp -> Attributes.map (\_ -> noOp)
    , htmlMap = \noOp -> Html.map (\_ -> noOp)
    }



---- CUSTOM VIEW


{-| TODO
-}
type CustomViewConfig a divider attributeNever htmlNever
    = CustomViewConfig (a -> String) (CustomViews a divider attributeNever htmlNever)


{-| TODO
-}
customViewConfig :
    (a -> String)
    -> CustomViews a divider attributeNever htmlNever
    -> CustomViewConfig a divider attributeNever htmlNever
customViewConfig =
    CustomViewConfig


{-| TODO
-}
type alias CustomViews a divider attributeNever htmlNever =
    { container : List attributeNever
    , button :
        { maybeSelection : Maybe a
        , open : Bool
        }
        ->
            { attributes : List attributeNever
            , children : List htmlNever
            }
    , ul : List attributeNever
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
    }


{-| TODO
-}
type alias DomFunctions attribute attributeNever html htmlNever msg =
    { div :
        List attribute
        ->
            { button : html
            , ul : html
            }
        -> html
    , text : String -> htmlNever
    , button : List attribute -> List html -> html
    , ul : List attribute -> List html -> html
    , li : List attribute -> List html -> html
    , on : String -> Decoder msg -> attribute
    , preventDefaultOn : String -> Decoder ( msg, Bool ) -> attribute
    , attribute : String -> String -> attribute
    , style : String -> String -> attributeNever
    , attributeMap : msg -> attributeNever -> attribute
    , htmlMap : msg -> htmlNever -> html
    }


{-| TODO
-}
customView :
    DomFunctions attribute attributeNever html htmlNever msg
    -> CustomViewConfig a divider attributeNever htmlNever
    -> Instance msg a
    -> List (Entry a divider)
    -> Dropdown
    -> Maybe a
    -> html
customView dom config instance allEntries (Dropdown data) maybeSelection =
    let
        (CustomViewConfig uniqueId views) =
            config

        buttonHtmlDetails =
            views.button
                { maybeSelection = maybeSelection
                , open = True
                }

        listboxConfig =
            Listbox.customViewConfig uniqueId
                { ul =
                    if data.open then
                        dom.style "position" "absolute" :: views.ul

                    else
                        dom.style "display" "none"
                            :: dom.style "position" "absolute"
                            :: views.ul
                , liOption = views.liOption
                , liDivider = views.liDivider
                , empty = dom.text ""
                , focusable = True
                }
    in
    dom.div
        ([ dom.on "mousedown" (Decode.succeed (instance.lift ListboxMousePressed))
         , dom.on "mouseup" (Decode.succeed (instance.lift (ListboxMouseReleased instance.id)))
         , dom.on "focusout"
            (Decode.at [ "target", "id" ] Decode.string
                |> Decode.andThen
                    (\targetId ->
                        if targetId == printButtonId instance.id then
                            Decode.fail "not handling that event here"

                        else
                            Decode.succeed (instance.lift (ListboxBlured instance.id))
                    )
            )
         , dom.on "keydown"
            (KeyInfo.decoder
                |> Decode.andThen
                    (\{ code, altDown, controlDown, metaDown, shiftDown } ->
                        case code of
                            "Escape" ->
                                if
                                    not altDown
                                        && not controlDown
                                        && not metaDown
                                        && not shiftDown
                                then
                                    Decode.succeed (instance.lift (ListboxEscapePressed instance.id))

                                else
                                    Decode.fail "not handling that key combination"

                            "Enter" ->
                                if
                                    not altDown
                                        && not controlDown
                                        && not metaDown
                                        && not shiftDown
                                then
                                    Decode.succeed (instance.lift (ListboxEnterPressed instance.id))

                                else
                                    Decode.fail "not handling that key combination"

                            _ ->
                                Decode.fail "not handling that key combination"
                    )
            )
         ]
            |> appendAttributes (dom.attributeMap (instance.lift NoOp)) views.container
        )
        { button = viewButton dom instance buttonHtmlDetails maybeSelection True
        , ul =
            ListboxUnique.customView
                { ul = dom.ul
                , li = dom.li
                , attribute = dom.attribute
                , on = dom.on
                , preventDefaultOn = dom.preventDefaultOn
                , attributeMap = dom.attributeMap
                , htmlMap = dom.htmlMap
                }
                listboxConfig
                { id = printListboxId instance.id
                , labelledBy = instance.labelledBy
                , lift = instance.lift << ListboxMsg (Just instance.id)
                }
                allEntries
                data.listbox
                maybeSelection
        }


viewButton :
    DomFunctions attribute attributeNever html htmlNever msg
    -> Instance msg a
    -> { attributes : List attributeNever, children : List htmlNever }
    -> Maybe a
    -> Bool
    -> html
viewButton dom instance { attributes, children } selection open =
    let
        { id, labelledBy, lift } =
            instance

        setAriaExpanded attrs =
            if open then
                dom.attribute "aria-expanded" "true" :: attrs

            else
                dom.attribute "aria-expanded" "false" :: attrs
    in
    dom.button
        ([ dom.attribute "id" (printButtonId id)
         , dom.attribute "type" "button"
         , dom.attribute "aria-labelledby" (printButtonId id ++ " " ++ labelledBy)
         , dom.attribute "aria-haspopup" "listbox"
         , dom.attribute "tabindex" "0"
         , dom.on "keypress"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        case key of
                            "Space" ->
                                Decode.succeed
                                    (lift (ButtonClicked { id = id, labelledBy = labelledBy }))

                            "Enter" ->
                                Decode.succeed
                                    (lift (ButtonClicked { id = id, labelledBy = labelledBy }))

                            _ ->
                                Decode.fail "not handling that key here"
                    )
            )
         , dom.on "click"
            (Decode.succeed
                (lift (ButtonClicked { id = id, labelledBy = labelledBy }))
            )
         , dom.on "keydown"
            (Decode.field "key" Decode.string
                |> Decode.andThen (buttonKeyDown instance)
            )
         ]
            |> setAriaExpanded
            |> appendAttributes (dom.attributeMap (lift NoOp))
                (dom.style "position" "relative" :: attributes)
        )
        (List.map (dom.htmlMap (lift NoOp)) children)


buttonKeyDown :
    Instance msg a
    -> String
    -> Decoder msg
buttonKeyDown { id, labelledBy, lift } code =
    let
        ids =
            { id = id
            , labelledBy = labelledBy
            }
    in
    case code of
        "ArrowUp" ->
            Decode.succeed (lift (ButtonArrowUpPressed ids))

        "ArrowDown" ->
            Decode.succeed (lift (ButtonArrowDownPressed ids))

        _ ->
            Decode.fail "not handling that key here"



-- IDS


printButtonId : String -> String
printButtonId id =
    id ++ "__button"


printListboxId : String -> String
printListboxId id =
    id ++ "-listbox"



---- UPDATE


{-| The dropdown's message type.
-}
type Msg a
    = NoOp
    | NextAnimationFrame { id : String, labelledBy : String }
      -- BUTTON
    | ButtonClicked { id : String, labelledBy : String }
    | ButtonArrowUpPressed { id : String, labelledBy : String }
    | ButtonArrowDownPressed { id : String, labelledBy : String }
      -- LISTBOX
    | ListboxMsg (Maybe String) (Listbox.Msg a)
    | ListboxEscapePressed String
    | ListboxEnterPressed String
    | ListboxBlured String
    | ListboxMousePressed
    | ListboxMouseReleased String


{-| Use this function to update the dropdown state. You have to provide the
same entries and selection as to your view function.

For example:

    update msg model =
        case msg of
            DropdownMsg dropdownMsg ->
                let
                    ( newDropdown, dropdownCmd, newSelection ) =
                        Listbox.Dropdown.update updateConfig
                            entries
                            dropdownMsg
                            model.dropdown
                            model.selection
                in
                ( { model
                    | dropdown = newDropdown
                    , selection = newSelection
                  }
                , Cmd.map DropdownMsg dropdownCmd
                )

In a more sofisticated example, the entries could be dynamic, as well. (For
example, loaded via an HTTP request.)

-}
update :
    UpdateConfig a
    -> List (Entry a divider)
    -> Msg a
    -> Dropdown
    -> Maybe a
    -> ( Dropdown, Cmd (Msg a), Maybe a )
update (UpdateConfig uniqueId behaviour) allEntries msg dropdown maybeSelection =
    let
        (Dropdown data) =
            dropdown

        listboxConfig =
            Listbox.updateConfig uniqueId
                { jumpAtEnds = behaviour.jumpAtEnds
                , separateFocus = behaviour.separateFocus
                , selectionFollowsFocus = behaviour.selectionFollowsFocus
                , handleHomeAndEnd = behaviour.handleHomeAndEnd
                , typeAhead = behaviour.typeAhead
                , minimalGap = behaviour.minimalGap
                , initialGap = behaviour.initialGap
                }
    in
    case msg of
        NoOp ->
            ( dropdown, Cmd.none, maybeSelection )

        NextAnimationFrame ids ->
            case data.pendingFocusListbox of
                Nothing ->
                    ( dropdown, Cmd.none, maybeSelection )

                Just id ->
                    ( Dropdown { data | pendingFocusListbox = Nothing }
                    , Task.attempt (\_ -> NoOp) <|
                        Listbox.focus
                            { id = printListboxId ids.id
                            , labelledBy = ids.labelledBy
                            , lift = ListboxMsg (Just ids.id)
                            }
                    , maybeSelection
                    )

        -- BUTTON
        ButtonClicked ids ->
            ( Dropdown { data | open = True }
            , Task.attempt (\_ -> NoOp) <|
                Listbox.focus
                    { id = printListboxId ids.id
                    , labelledBy = ids.labelledBy
                    , lift = ListboxMsg (Just ids.id)
                    }
            , maybeSelection
            )

        ButtonArrowUpPressed ids ->
            let
                ( newListbox, newSelection ) =
                    ListboxUnique.focusPreviousOrFirstEntry listboxConfig
                        allEntries
                        data.listbox
                        maybeSelection
            in
            ( Dropdown
                { data
                    | open = True
                    , listbox = newListbox
                    , pendingFocusListbox =
                        if data.open then
                            Nothing

                        else
                            Just ids.id
                }
            , Task.attempt (\_ -> NoOp) <|
                Listbox.focus
                    { id = printListboxId ids.id
                    , labelledBy = ids.labelledBy
                    , lift = ListboxMsg (Just ids.id)
                    }
            , newSelection
            )

        ButtonArrowDownPressed ids ->
            let
                ( newListbox, newSelection ) =
                    ListboxUnique.focusNextOrFirstEntry listboxConfig
                        allEntries
                        data.listbox
                        maybeSelection
            in
            ( Dropdown
                { data
                    | open = True
                    , listbox = newListbox
                    , pendingFocusListbox =
                        if data.open then
                            Nothing

                        else
                            Just ids.id
                }
            , Task.attempt (\_ -> NoOp) <|
                Listbox.focus
                    { id = printListboxId ids.id
                    , labelledBy = ids.labelledBy
                    , lift = ListboxMsg (Just ids.id)
                    }
            , newSelection
            )

        ListboxMsg maybeId listboxMsg ->
            let
                ( newListbox, listboxCmd, newSelection ) =
                    ListboxUnique.update listboxConfig
                        allEntries
                        listboxMsg
                        data.listbox
                        maybeSelection
            in
            ( Dropdown { data | listbox = newListbox }
            , Cmd.map (ListboxMsg maybeId) listboxCmd
            , newSelection
            )

        ListboxEscapePressed id ->
            ( Dropdown { data | open = False }
            , focusButton id
            , maybeSelection
            )

        ListboxEnterPressed id ->
            if data.open then
                ( Dropdown { data | open = False }
                , focusButton id
                , maybeSelection
                )

            else
                ( dropdown, Cmd.none, maybeSelection )

        ListboxBlured id ->
            if data.preventBlur then
                ( dropdown, Cmd.none, maybeSelection )

            else
                ( Dropdown { data | open = False }
                , Cmd.none
                , maybeSelection
                )

        ListboxMousePressed ->
            ( Dropdown { data | preventBlur = True }
            , Cmd.none
            , maybeSelection
            )

        ListboxMouseReleased id ->
            if behaviour.closeAfterMouseSelection then
                ( Dropdown { data | open = False, preventBlur = False }
                , focusButton id
                , maybeSelection
                )

            else
                ( Dropdown { data | preventBlur = False }
                , Cmd.none
                , maybeSelection
                )



-- CMDS


focusButton : String -> Cmd (Msg a)
focusButton id =
    Dom.focus (printButtonId id)
        |> Task.attempt (\_ -> NoOp)



---- SUBSCRIPTIONS


{-| Do not forget to add this to your subscriptions:

    subscriptions model =
        Sub.map DropdownMsg
            (Listbox.Dropdown.subscriptions model.dropdown)

-}
subscriptions : Dropdown -> Sub (Msg a)
subscriptions (Dropdown data) =
    Sub.batch
        [ if data.open then
            Sub.map (ListboxMsg Nothing) (Listbox.subscriptions data.listbox)

          else
            Sub.none
        ]



-- MISC


appendAttributes :
    (attributeNever -> attribute)
    -> List attributeNever
    -> List attribute
    -> List attribute
appendAttributes attributeMap neverAttrs attrs =
    neverAttrs
        |> List.map attributeMap
        |> List.append attrs


preventDefault : Decoder msg -> Decoder ( msg, Bool )
preventDefault decoder =
    decoder
        |> Decode.map (\msg -> ( msg, True ))


allowDefault : Decoder msg -> Decoder ( msg, Bool )
allowDefault decoder =
    decoder
        |> Decode.map (\msg -> ( msg, False ))
