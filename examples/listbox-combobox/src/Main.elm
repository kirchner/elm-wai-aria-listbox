module Main exposing (main)

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

import Accessibility.Widget as Widget
import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Lazy as Html
import Listbox exposing (Entry, HtmlDetails)
import Listbox.Combobox as Combobox exposing (Combobox)


main : Program {} Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



---- MODEL


type alias Model =
    { combobox : Combobox String
    , value : String
    }


init : {} -> ( Model, Cmd Msg )
init _ =
    ( { combobox = Combobox.init
      , value = ""
      }
    , Cmd.none
    )



---- UPDATE


type Msg
    = ComboboxMsg (Combobox.Msg String)


update : Msg -> Model -> ( Model, Cmd Msg )
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
                        |> List.filter (String.contains model.value)
                        |> List.map Listbox.option
            in
            ( { model
                | combobox = newCombobox
                , value = newValue
              }
            , Cmd.map ComboboxMsg comboboxCmd
            )



---- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ComboboxMsg (Combobox.subscriptions model.combobox)



---- VIEW


view : Model -> Html Msg
view model =
    let
        matchingFruits =
            fruits
                |> List.filter (String.contains model.value)
                |> List.map Listbox.option
    in
    Html.div
        [ Attributes.class "section" ]
        [ Html.div
            [ Attributes.class "container" ]
            [ Html.div
                [ Attributes.class "field" ]
                [ Html.label
                    [ Attributes.id "fruits-label"
                    , Attributes.for "fruits"
                    ]
                    [ Html.text "Fruits" ]
                , Html.div
                    [ Attributes.class "control" ]
                    [ Combobox.view viewConfig
                        { id = "fruits"
                        , label = Listbox.labelledBy "fruits-label"
                        , lift = ComboboxMsg
                        }
                        matchingFruits
                        model.combobox
                        model.value
                    ]
                ]
            ]
        ]



---- CONFIG


updateConfig : Combobox.UpdateConfig String
updateConfig =
    Combobox.updateConfig
        { uniqueId = identity
        , behaviour =
            { entryToValue = identity
            , clearOnEscape = True
            , separateFocus = True
            , handleHomeAndEnd = True
            , minimalGap = 0
            , initialGap = 0
            }
        }


viewConfig : Combobox.ViewConfig String Never
viewConfig =
    Combobox.viewConfig
        { uniqueId = identity
        , role = Combobox.Textbox
        , visibility =
            Combobox.Sometimes
                { whenMatchingWithMinimalValueLength = Just 1
                , whenRequested = Nothing
                }
        , views =
            { container = []
            , input = [ Attributes.class "input" ]
            , ul = [ Attributes.class "list" ]
            , liOption =
                \{ selected, focused, hovered, maybeQuery } name ->
                    { attributes =
                        [ Attributes.class "entry"
                        , Attributes.classList
                            [ ( "entry--mouse-focused", hovered )
                            , ( "entry--keyboard-focused", focused )
                            ]
                        ]
                    , children =
                        [ Html.text name ]
                    }
            , liDivider = Listbox.noDivider
            }
        }



---- DATA


fruits : List String
fruits =
    [ "Açaí"
    , "Apple"
    , "Akee"
    , "Apricot"
    , "Avocado"
    , "Banana"
    , "Bilberry"
    , "Blackberry"
    , "Blackcurrant"
    , "Black sapote"
    , "Blueberry"
    , "Boysenberry"
    , "Buddha's hand (fingered citron)"
    , "Crab apples"
    , "Currant"
    , "Cherry"
    , "Cherimoya (Custard Apple)"
    , "Chico fruit"
    , "Cloudberry"
    , "Coconut"
    , "Cranberry"
    , "Cucumber"
    , "Damson"
    , "Date"
    , "Dragonfruit (or Pitaya)"
    , "Durian"
    , "Elderberry"
    , "Feijoa"
    , "Fig"
    , "Goji berry"
    , "Gooseberry"
    , "Grape"
    , "Grapefruit"
    , "Guava"
    , "Honeyberry"
    , "Huckleberry"
    , "Jabuticaba"
    , "Jackfruit"
    , "Jambul"
    , "Japanese plum"
    , "Jostaberry"
    , "Jujube"
    , "Juniper berry"
    , "Kiwano (horned melon)"
    , "Kiwifruit"
    , "Kumquat"
    , "Lemon"
    , "Lime"
    , "Loquat"
    , "Longan"
    , "Lychee"
    , "Mango"
    , "Mangosteen"
    , "Marionberry"
    , "Melon"
    , "Miracle fruit"
    , "Mulberry"
    , "Nectarine"
    , "Nance"
    , "Olive"
    , "Orange"
    , "Papaya"
    , "Passionfruit"
    , "Peach"
    , "Pear"
    , "Persimmon"
    , "Plantain"
    , "Plum"
    , "Pineapple"
    , "Pineberry"
    , "Plumcot (or Pluot)"
    , "Pomegranate"
    , "Pomelo"
    , "Purple mangosteen"
    , "Quince"
    , "Raspberry"
    , "Rambutan (or Mamin Chino)"
    , "Redcurrant"
    , "Salal berry"
    , "Salak"
    , "Satsuma"
    , "Soursop"
    , "Star apple"
    , "Star fruit"
    , "Strawberry"
    , "Surinam cherry"
    , "Tamarillo"
    , "Tamarind"
    , "Ugli fruit"
    , "Yuzu"
    ]
