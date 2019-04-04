module Listbox.Unique exposing
    ( view, update
    , focusEntry, focusNextOrFirstEntry, focusPreviousOrFirstEntry
    , customView
    )

{-| This is a variant of `Listbox` allowing only **at most one**
selection. You just have to replace the `view` and the `update` function with
the ones in this module.

@docs view, update

@docs focusEntry, focusNextOrFirstEntry, focusPreviousOrFirstEntry

@docs customView

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
import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Internal.Listbox as Internal exposing (DomFunctions)
import Json.Decode exposing (Decoder)
import Listbox exposing (CustomViewConfig, Entry, Listbox, Msg, UpdateConfig, ViewConfig)


{-| Use this instead of `Listbox.view` if the user can only select **at
most one** entry in the listbox. The only difference between the type signature
of this function and the one of `Listbox.view` is that the last argument is
a `Maybe a` instead of a `List a`.
-}
view :
    ViewConfig a divider
    ->
        { id : String
        , labelledBy : String
        , lift : Msg a -> msg
        }
    -> List (Entry a divider)
    -> Listbox
    -> Maybe a
    -> Html msg
view config cfg entries listbox selection =
    Internal.view False htmlFunctions config cfg entries listbox (maybeToList selection)


htmlFunctions : DomFunctions (Attribute msg) (Attribute Never) (Html msg) (Html Never) msg
htmlFunctions =
    { ul = Html.ul
    , li = Html.li
    , on = Events.on
    , preventDefaultOn = Events.preventDefaultOn
    , attribute = Attributes.attribute
    , htmlMap = \noOp -> Html.map (\_ -> noOp)
    , attributeMap = \noOp -> Attributes.map (\_ -> noOp)
    }


{-| Use this instead of `Listbox.customView` if the user can only select **at
most one** entry in the listbox. The only difference between the type signature
of this function and the one of `Listbox.customView` is that the last argument
is a `Maybe a` instead of a `List a`.
-}
customView :
    DomFunctions attribute attributeNever html htmlNever msg
    -> CustomViewConfig a divider attributeNever htmlNever
    ->
        { id : String
        , labelledBy : String
        , lift : Msg a -> msg
        }
    -> List (Entry a divider)
    -> Listbox
    -> Maybe a
    -> html
customView dom config cfg entries listbox selection =
    Internal.view False dom config cfg entries listbox (maybeToList selection)


{-| Use this function instead of `Listbox.update` if the user can only
select **at most one** entry in the listbox. The only difference between the
type signature of this function and the one of `Listbox.update` is that the
last argument is a `Maybe a` instead of a `List a`.
-}
update :
    UpdateConfig a
    -> List (Entry a divider)
    -> Msg a
    -> Listbox
    -> Maybe a
    -> ( Listbox, Cmd (Msg a), Maybe a )
update config allEntries msg listbox selection =
    let
        ( newListbox, cmd, newSelection ) =
            Listbox.update config allEntries msg listbox <|
                maybeToList selection
    in
    ( newListbox, cmd, listToMaybe newSelection )


{-| Sets the keyboard focus to the provided options.

**Note**: This will not adjust the scroll position of the listbox, so you might
want to apply `scrollToFocus` afterwards.

-}
focusEntry : UpdateConfig a -> a -> Listbox -> Maybe a -> ( Listbox, Maybe a )
focusEntry config newEntry listbox selection =
    withUnique selection (Listbox.focusEntry config newEntry listbox)


{-| Sets the keyboard focus to the next option. If `jumpAtEnds` is true and the
focus is already on the last option, the first option is selected.

**Note**: This will not adjust the scroll position of the listbox, so you might
want to apply `scrollToFocus` afterwards.

-}
focusNextOrFirstEntry :
    UpdateConfig a
    -> List (Entry a divider)
    -> Listbox
    -> Maybe a
    -> ( Listbox, Maybe a )
focusNextOrFirstEntry config allEntries listbox selection =
    withUnique selection (Listbox.focusNextOrFirstEntry config allEntries listbox)


{-| Sets the keyboard focus to the previous option. If `jumpAtEnds` is true and the
focus is already on the first option, the first option is selected.

**Note**: This will not adjust the scroll position of the listbox, so you might
want to apply `scrollToFocus` afterwards.

-}
focusPreviousOrFirstEntry :
    UpdateConfig a
    -> List (Entry a divider)
    -> Listbox
    -> Maybe a
    -> ( Listbox, Maybe a )
focusPreviousOrFirstEntry config allEntries listbox selection =
    withUnique selection (Listbox.focusPreviousOrFirstEntry config allEntries listbox)



---- HELPER


withUnique :
    Maybe a
    -> (List a -> ( Listbox, List a ))
    -> ( Listbox, Maybe a )
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
