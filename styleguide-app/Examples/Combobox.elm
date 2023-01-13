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
                combobox =
                    Combobox.update comboboxMsg state.combobox
            in
            ( { state | combobox = combobox }
            , Cmd.none
            )

        UpdateControls settings ->
            ( { state | settings = settings }, Cmd.none )


viewExampleWithCode : State -> Settings -> ( String, Html Msg )
viewExampleWithCode state settings =
    ( "", Html.map ComboboxMsg (Combobox.view state.combobox) )
