module Examples.Combobox exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled as Html exposing (..)
import Category exposing (Category(..))
import CheckboxIcons
import Code
import Css exposing (Style)
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled.Attributes exposing (css)
import KeyboardSupport exposing (Key(..))
import Nri.Ui.Checkbox.V7 as Checkbox
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Combobox.V1 as Combobox
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Table.V6 as Table


moduleName : String
moduleName =
    "Combobox"


version : Int
version =
    1


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview = preview
    , view =
        \ellieLinkConfig state ->
            let
                settings =
                    Control.currentValue state.settings

                ( exampleCode, exampleView ) =
                    viewExampleWithCode state settings
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControls
                , settings = state.settings
                , mainType = Nothing
                , extraCode = []
                , renderExample = Code.unstyledView
                , toExampleCode = \_ -> [ { sectionName = "Example", code = exampleCode } ]
                }
            , Heading.h2 [ Heading.plaintext "Customizable example" ]
            , exampleView
            ]
    , categories = [ Inputs ]
    , keyboardSupport = []
    }



-- TODO


preview =
    []


{-| -}
type alias State =
    { combobox : Combobox.Model
    , settings : Control Settings
    }


{-| -}
init : State
init =
    { combobox =
        Combobox.init "combobox"
            "Options"
            10
            [ { value = "foo", label = "Foo" }
            , { value = "bar", label = "Bar" }
            , { value = "baz", label = "Baz" }
            , { value = "foo-bar-baz", label = "Foo Bar Baz" }
            , { value = "item-3", label = "Item 1" }
            , { value = "item-2", label = "Item 2" }
            , { value = "item-1", label = "Item 3" }
            , { value = "item-4", label = "Item 4" }
            , { value = "item-5", label = "Item 5" }
            , { value = "item-6", label = "Item 6" }
            , { value = "item-7", label = "Item 7" }
            , { value = "item-8", label = "Item 8" }
            , { value = "item-9", label = "Item 9" }
            , { value = "item-10", label = "Item 10" }
            , { value = "item-11", label = "Item 11" }
            , { value = "item-12", label = "Item 12" }
            , { value = "item-13", label = "Item 13" }
            , { value = "item-14", label = "Item 14" }
            , { value = "item-15", label = "Item 15" }
            , { value = "item-16", label = "Item 16" }
            , { value = "item-17", label = "Item 17" }
            , { value = "item-18", label = "Item 18" }
            , { value = "item-19", label = "Item 19" }
            , { value = "item-20", label = "Item 20" }
            , { value = "item-21", label = "Item 21" }
            , { value = "item-22", label = "Item 22" }
            , { value = "item-23", label = "Item 23" }
            , { value = "item-24", label = "Item 24" }
            , { value = "item-25", label = "Item 25" }
            , { value = "item-26", label = "Item 26" }
            , { value = "item-27", label = "Item 27" }
            , { value = "item-28", label = "Item 28" }
            , { value = "item-29", label = "Item 29" }
            , { value = "item-30", label = "Item 30" }
            , { value = "item-31", label = "Item 31" }
            , { value = "item-32", label = "Item 32" }
            , { value = "item-33", label = "Item 33" }
            , { value = "item-34", label = "Item 34" }
            , { value = "item-35", label = "Item 35" }
            , { value = "item-36", label = "Item 36" }
            , { value = "item-37", label = "Item 37" }
            , { value = "item-38", label = "Item 38" }
            , { value = "item-39", label = "Item 39" }
            , { value = "item-40", label = "Item 40" }
            , { value = "item-41", label = "Item 41" }
            , { value = "item-42", label = "Item 42" }
            , { value = "item-43", label = "Item 43" }
            , { value = "item-44", label = "Item 44" }
            , { value = "item-45", label = "Item 45" }
            , { value = "item-46", label = "Item 46" }
            , { value = "item-47", label = "Item 47" }
            , { value = "item-48", label = "Item 48" }
            , { value = "item-49", label = "Item 49" }
            , { value = "item-50", label = "Item 50" }
            , { value = "item-51", label = "Item 51" }
            , { value = "item-52", label = "Item 52" }
            , { value = "item-53", label = "Item 53" }
            , { value = "item-54", label = "Item 54" }
            , { value = "item-55", label = "Item 55" }
            , { value = "item-56", label = "Item 56" }
            , { value = "item-57", label = "Item 57" }
            , { value = "item-58", label = "Item 58" }
            , { value = "item-59", label = "Item 59" }
            , { value = "item-60", label = "Item 60" }
            , { value = "item-61", label = "Item 61" }
            , { value = "item-62", label = "Item 62" }
            , { value = "item-63", label = "Item 63" }
            , { value = "item-64", label = "Item 64" }
            , { value = "item-65", label = "Item 65" }
            , { value = "item-66", label = "Item 66" }
            , { value = "item-67", label = "Item 67" }
            , { value = "item-68", label = "Item 68" }
            , { value = "item-69", label = "Item 69" }
            , { value = "item-70", label = "Item 70" }
            , { value = "item-71", label = "Item 71" }
            , { value = "item-72", label = "Item 72" }
            , { value = "item-73", label = "Item 73" }
            , { value = "item-74", label = "Item 74" }
            , { value = "item-75", label = "Item 75" }
            , { value = "item-76", label = "Item 76" }
            , { value = "item-77", label = "Item 77" }
            , { value = "item-78", label = "Item 78" }
            , { value = "item-79", label = "Item 79" }
            , { value = "item-80", label = "Item 80" }
            , { value = "item-81", label = "Item 81" }
            , { value = "item-82", label = "Item 82" }
            , { value = "item-83", label = "Item 83" }
            , { value = "item-84", label = "Item 84" }
            , { value = "item-85", label = "Item 85" }
            ]
    , settings = controlSettings
    }


type alias Settings =
    { label : String
    }


controlSettings : Control Settings
controlSettings =
    Control.record Settings
        |> Control.field "label" (Control.string "Foo-Bar-Baz Combobox")


{-| -}
type Msg
    = ComboboxMsg Combobox.Msg
    | UpdateControls (Control Settings)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ComboboxMsg comboboxMsg ->
            let
                (newCombobox, cmd) = Combobox.update comboboxMsg state.combobox
            in
            ( { state | combobox = newCombobox }
            , Cmd.map ComboboxMsg cmd
            )

        UpdateControls settings ->
            ( { state | settings = settings }, Cmd.none )


viewExampleWithCode : State -> Settings -> ( String, Html Msg )
viewExampleWithCode state settings =
    ( "", Html.map ComboboxMsg (Combobox.view state.combobox) )
