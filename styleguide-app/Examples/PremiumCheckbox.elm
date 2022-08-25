module Examples.PremiumCheckbox exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import CheckboxIcons
import Code
import CommonControls
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import KeyboardSupport exposing (Key(..))
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Pennant.V2 as Pennant
import Nri.Ui.PremiumCheckbox.V8 as PremiumCheckbox
import Nri.Ui.Svg.V1 as Svg
import Set exposing (Set)


moduleName : String
moduleName =
    "PremiumCheckbox"


version : Int
version =
    8


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
                , mainType = Just "RootHtml.Html Msg"
                , extraCode =
                    [ "import Nri.Ui.Data.PremiumDisplay as PremiumDisplay"
                    , "\n\n"
                    , "type Msg = ToggleCheck Bool | ClickedPremiumLock"
                    ]
                , toExampleCode = \_ -> [ { sectionName = "view", code = exampleCode } ]
                }
            , Heading.h2 [ Heading.plaintext "Example" ]
            , exampleView
            ]
    , categories = [ Inputs ]
    , keyboardSupport =
        [ { keys = [ Space ]
          , result = "Select or deselect the checkbox (may cause page scroll)"
          }
        ]
    }


preview : List (Html Never)
preview =
    let
        renderPreview ( icon, label_ ) =
            span
                [ css
                    [ Css.color Colors.navy
                    , Css.fontSize (Css.px 15)
                    , Fonts.baseFont
                    , Css.fontWeight (Css.int 600)
                    , Css.displayFlex
                    , Css.alignItems Css.center
                    ]
                ]
                [ Pennant.premiumFlag
                    |> Svg.withCss [ Css.marginRight (Css.px 8) ]
                    |> Svg.withWidth (Css.px 25)
                    |> Svg.withHeight (Css.px 30)
                    |> Svg.toHtml
                , Svg.toHtml (Svg.withCss [ Css.marginRight (Css.px 8) ] icon)
                , text label_
                ]
    in
    [ ( CheckboxIcons.lockOnInside "lockOnInside-preview-lockOnInside", "Locked" )
    , ( CheckboxIcons.unchecked "unchecked-preview-unchecked", "Unchecked" )
    , ( CheckboxIcons.checked "checkbox-preview-checked", "Checked" )
    ]
        |> List.map renderPreview


{-| -}
type alias State =
    { isChecked : Set String
    , settings : Control Settings
    }


{-| -}
init : State
init =
    { isChecked = Set.empty
    , settings = controlSettings
    }


type alias Settings =
    { label : String
    , attributes : List ( String, PremiumCheckbox.Attribute Msg )
    }


controlSettings : Control Settings
controlSettings =
    Control.record Settings
        |> Control.field "label" (Control.string "Identify Adjectives 1")
        |> Control.field "attributes" controlAttributes


controlAttributes : Control (List ( String, PremiumCheckbox.Attribute Msg ))
controlAttributes =
    ControlExtra.list
        |> ControlExtra.optionalListItem "premiumDisplay"
            (Control.map
                (\( str, val ) -> ( "PremiumCheckbox.premium " ++ str, PremiumCheckbox.premium val ))
                CommonControls.premiumDisplay
            )
        |> ControlExtra.optionalListItem "onLockedClick"
            (Control.value
                ( "PremiumCheckbox.onLockedClick ClickedPremiumLock"
                , PremiumCheckbox.onLockedClick ClickedPremiumLock
                )
            )
        |> ControlExtra.optionalBoolListItem "PremiumCheckbox.disabled" ( "PremiumCheckbox.disabled", PremiumCheckbox.disabled )
        |> CommonControls.css_ "setCheckboxContainerCss"
            ( "[ Css.border3 (Css.px 4) Css.dashed Colors.red ]"
            , [ Css.border3 (Css.px 4) Css.dashed Colors.red ]
            )
            { moduleName = moduleName
            , use = PremiumCheckbox.setCheckboxContainerCss
            }
        |> CommonControls.css_ "setCheckboxEnabledLabelCss"
            ( "[ Css.border3 (Css.px 4) Css.dotted Colors.orange ]"
            , [ Css.border3 (Css.px 4) Css.dotted Colors.orange ]
            )
            { moduleName = moduleName
            , use = PremiumCheckbox.setCheckboxEnabledLabelCss
            }
        |> CommonControls.css_ "setCheckboxDisabledLabelCss"
            ( "[ Css.textDecoration Css.lineThrough ]"
            , [ Css.textDecoration Css.lineThrough ]
            )
            { moduleName = moduleName
            , use = PremiumCheckbox.setCheckboxDisabledLabelCss
            }


viewExampleWithCode : State -> Settings -> ( String, Html Msg )
viewExampleWithCode state settings =
    let
        id =
            "unique-premium-example-id"
    in
    ( [ "PremiumCheckbox.view "
      , Code.record
            [ ( "label", Code.string settings.label )
            , ( "onChange", "ToggleCheck" )
            ]
      , Code.listMultiline
            (("PremiumCheckbox.selected " ++ Code.bool (Set.member id state.isChecked))
                :: List.map Tuple.first settings.attributes
            )
            1
      ]
        |> String.join ""
    , PremiumCheckbox.view
        { label = settings.label
        , onChange = ToggleCheck id
        }
        ([ [ PremiumCheckbox.onLockedClick ClickedPremiumLock
           , PremiumCheckbox.selected (Set.member id state.isChecked)
           ]
         , List.map Tuple.second settings.attributes
         ]
            |> List.concat
        )
    )


{-| -}
type Msg
    = ToggleCheck Id Bool
    | UpdateControls (Control Settings)
    | ClickedPremiumLock


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ToggleCheck id checked ->
            let
                isChecked =
                    if checked then
                        Set.insert id state.isChecked

                    else
                        Set.remove id state.isChecked
            in
            ( { state | isChecked = isChecked }, Cmd.none )

        UpdateControls settings ->
            ( { state | settings = settings }, Cmd.none )

        ClickedPremiumLock ->
            ( Debug.log moduleName "clicked a premium lock" |> always state, Cmd.none )


type alias Id =
    String
