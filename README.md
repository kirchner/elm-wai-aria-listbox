# elm-wai-aria-listbox

[![Build Status](https://travis-ci.org/kirchner/elm-wai-aria-listbox.svg?branch=master)](https://travis-ci.org/kirchner/elm-wai-aria-listbox)

This package offers an implementation of the [Listbox
widget](https://www.w3.org/TR/wai-aria-practices-1.1/#Listbox) as specified in
the [WAI-ARIA Authoring Practices
1.1](https://www.w3.org/TR/wai-aria-practices-1.1/).  Take a look at the [demo
page](https://kirchner.github.io/elm-wai-aria-listbox/).

See more end-to-end example code in the `examples/` folder.


## Overview

The minimal code to get a working collapsible listbox dropdown menu with
autoscrolling and type ahead functionallity, would be something like this:

```elm
import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Lazy as Html
import Listbox as Listbox exposing (Entry, HtmlDetails)
import Listbox.Dropdown as Dropdown exposing (Dropdown)


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
    { dropdown : Dropdown
    , selection : Maybe String
    }


init : {} -> ( Model, Cmd Msg )
init _ =
    ( { dropdown = Dropdown.init
      , selection = Nothing
      }
    , Cmd.none
    )



---- UPDATE


type Msg
    = NoOp
    | DropdownMsg (Dropdown.Msg String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DropdownMsg dropdownMsg ->
            let
                ( newDropdown, dropdownCmd, newSelection ) =
                    Dropdown.update updateConfig
                        fruits
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



---- SUBSCRIPTIONS


subscriptions model =
    Sub.map DropdownMsg (Dropdown.subscriptions model.dropdown)



---- VIEW


view model =
    Html.div []
        [ Html.label
            [ Attributes.id "fruits-label" ]
            [ Html.text "Fruits" ]
        , Dropdown.view viewConfig
            { id = "fruits"
            , labelledBy = "fruits-label"
            , lift = DropdownMsg
            }
            fruits
            model.dropdown
            model.selection
        ]



---- CONFIG


updateConfig : Dropdown.UpdateConfig String
updateConfig =
    Dropdown.updateConfig identity
        { jumpAtEnds = True
        , closeAfterMouseSelection = False
        , separateFocus = True
        , selectionFollowsFocus = False
        , handleHomeAndEnd = True
        , typeAhead = Listbox.simpleTypeAhead 200 identity
        , minimalGap = 0
        , initialGap = 0
        }


viewConfig : Dropdown.ViewConfig String Never
viewConfig =
    Dropdown.viewConfig identity
        { container = []
        , button =
            \{ maybeSelection, open } ->
                { attributes = []
                , children =
                    [ maybeSelection
                        |> Maybe.withDefault "Select a fruit..."
                        |> Html.text
                    ]
                }
        , ul = []
        , liOption =
            \{ selected, focused } name ->
                { attributes =
                    [ if focused then
                        Attributes.style "background-color" "green"
                      else
                        Attributes.style "background-color" "white"
                    , if selected then
                        Attributes.style "color" "blue"
                      else
                        Attributes.style "color" "black"
                    ]
                , children = [ Html.text name ]
                }
        , liDivider = Listbox.noDivider
        }



---- DATA


fruits : List (Entry String divider)
fruits =
    List.map Listbox.option
        [ "Apple", "Banana", "Cherry" ]
```


## Usage

Add [the kirchner/elm-wai-aria-listbox Elm
package](https://package.elm-lang.org/packages/kirchner/elm-wai-aria-listbox/latest)
as a dependency by running

```
$ elm install kirchner/elm-wai-aria-listbox
```
