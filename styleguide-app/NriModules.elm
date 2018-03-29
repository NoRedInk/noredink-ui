module NriModules exposing (ModuleStates, Msg, init, nriThemedModules, styles, subscriptions, update)

import DEPRECATED.Css.File exposing (Stylesheet, compile, stylesheet)
import Examples.Colors
import Examples.Fonts
import Examples.Icon
import Examples.SegmentedControl
import Examples.Text
import Examples.Text.Writing
import Examples.TextArea as TextAreaExample
import Html exposing (Html, img)
import Html.Attributes exposing (..)
import ModuleExample exposing (Category(..), ModuleExample)
import Navigation
import Nri.Ui.AssetPath as AssetPath exposing (Asset(Asset))
import Nri.Ui.Icon.V1
import Nri.Ui.SegmentedControl.V2
import Nri.Ui.Text.V1 as Text
import Nri.Ui.TextArea.V1 as TextArea
import String.Extra


type alias ModuleStates =
    { segmentedControlState : Examples.SegmentedControl.State
    , textAreaExampleState : TextAreaExample.State
    }


init : ModuleStates
init =
    { segmentedControlState = Examples.SegmentedControl.init
    , textAreaExampleState = TextAreaExample.init
    }


type Msg
    = SegmentedControlMsg Examples.SegmentedControl.Msg
    | ShowItWorked String String
    | TextAreaExampleMsg TextAreaExample.Msg
    | NoOp


update : Msg -> ModuleStates -> ( ModuleStates, Cmd Msg )
update msg moduleStates =
    case msg of
        SegmentedControlMsg msg ->
            let
                ( segmentedControlState, cmd ) =
                    Examples.SegmentedControl.update msg moduleStates.segmentedControlState
            in
            ( { moduleStates | segmentedControlState = segmentedControlState }
            , Cmd.map SegmentedControlMsg cmd
            )

        ShowItWorked group message ->
            let
                _ =
                    Debug.log group message
            in
            ( moduleStates, Cmd.none )

        TextAreaExampleMsg msg ->
            let
                ( textAreaExampleState, cmd ) =
                    TextAreaExample.update msg moduleStates.textAreaExampleState
            in
            ( { moduleStates | textAreaExampleState = textAreaExampleState }
            , Cmd.map TextAreaExampleMsg cmd
            )

        NoOp ->
            ( moduleStates, Cmd.none )


subscriptions : ModuleStates -> Sub Msg
subscriptions moduleStates =
    Sub.batch
        []


{-| A container with a visually-apparent size for demonstrating how style guide components
fill their parents.
-}
container : Int -> List (Html msg) -> Html msg
container width children =
    Html.div
        [ Html.Attributes.class "demo-container"
        , style [ ( "width", toString width ++ "px" ) ]
        ]
        children


nriThemedModules : ModuleStates -> List (ModuleExample Msg)
nriThemedModules model =
    [ Examples.Icon.example assets
    , Examples.SegmentedControl.example SegmentedControlMsg model.segmentedControlState
    , Examples.Text.example
    , Examples.Text.Writing.example
    , Examples.Fonts.example
    , TextAreaExample.example TextAreaExampleMsg model.textAreaExampleState
    , Examples.Colors.example
    ]


exampleMessages : (msg -> Msg) -> String -> ModuleExample.ModuleMessages msg Msg
exampleMessages exampleMessageWrapper exampleName =
    { noOp = NoOp
    , showItWorked = ShowItWorked exampleName
    , wrapper = exampleMessageWrapper
    }


route : Navigation.Location -> Maybe String
route location =
    location.hash
        |> String.dropLeft 1
        |> String.Extra.nonEmpty


styles : List Stylesheet
styles =
    List.concat
        [ -- NOTE: these will go away as the modules' styles are integrated with Nri.Css.Site.elm
          [ ModuleExample.styles
          ]
        , (Examples.Icon.styles |> .css) ()
        , (Nri.Ui.Icon.V1.styles |> .css) ()
        , (Nri.Ui.SegmentedControl.V2.styles |> .css) ()
        , (Text.styles |> .css) ()
        , (TextArea.styles |> .css) assets
        ]


type alias Assets =
    { activity : String
    , arrowDown : String
    , attention_svg : Asset
    , bulb : String
    , calendar : String
    , checkmark : String
    , class : String
    , clever : String
    , clock : String
    , commentNotStarred_png : Asset
    , commentStarred_png : Asset
    , compass : String
    , darkBlueCheckmark_svg : Asset
    , diagnostic : String
    , document : String
    , download : String
    , edit : String
    , editWriting : String
    , exclamation : String
    , exclamationPoint_svg : Asset
    , facebookBlue_svg : Asset
    , flipper : String
    , footsteps : String
    , gear : String
    , hint_png : Asset
    , iconCalendar_svg : Asset
    , iconCheck_png : Asset
    , iconFlag_png : Asset
    , icons_arrowDownBlue_svg : Asset
    , icons_arrowRightBlue_svg : Asset
    , icons_clockRed_svg : Asset
    , icons_equals_svg : Asset
    , icons_helpBlue_svg : Asset
    , icons_peerReview_svg : Asset
    , icons_plusBlue_svg : Asset
    , icons_quickWrite_svg : Asset
    , icons_searchGray_svg : Asset
    , icons_xBlue_svg : Asset
    , icons_xBlue_svg : Asset
    , key : String
    , leaderboard : String
    , leftArrowBlue_png : Asset
    , level1Badge_png : Asset
    , level2Badge_png : Asset
    , level3Badge_png : Asset
    , lock : String
    , logoRedBlack_svg : Asset
    , newspaper : String
    , openClose : String
    , performance : String
    , personBlue_svg : Asset
    , practice : String
    , premiumLock_svg : Asset
    , preview : String
    , quiz : String
    , seemore : String
    , share : String
    , smallCheckmark_png : Asset
    , sort : String
    , sortArrow : String
    , speedometer : String
    , squiggly_png : Asset
    , startingOffBadge_png : Asset
    , teach_assignments_copyWhite_svg : Asset
    , twitterBlue_svg : Asset
    , unarchiveBlue2x_png : Asset
    , writingAssignment : String
    , writingcycle : String
    , x : String
    , xWhite_svg : Asset
    }


assets : Assets
assets =
    { activity = ""
    , arrowDown = ""
    , attention_svg = Asset ""
    , bulb = ""
    , calendar = ""
    , checkmark = ""
    , class = ""
    , clever = ""
    , clock = ""
    , commentNotStarred_png = Asset ""
    , commentStarred_png = Asset ""
    , compass = ""
    , darkBlueCheckmark_svg = Asset ""
    , diagnostic = ""
    , document = ""
    , download = ""
    , edit = ""
    , editWriting = ""
    , exclamation = ""
    , exclamationPoint_svg = Asset ""
    , facebookBlue_svg = Asset ""
    , flipper = ""
    , footsteps = ""
    , gear = ""
    , hint_png = Asset ""
    , iconCalendar_svg = Asset ""
    , iconCheck_png = Asset ""
    , iconFlag_png = Asset ""
    , icons_arrowDownBlue_svg = Asset ""
    , icons_arrowRightBlue_svg = Asset ""
    , icons_clockRed_svg = Asset ""
    , icons_equals_svg = Asset ""
    , icons_helpBlue_svg = Asset ""
    , icons_peerReview_svg = Asset ""
    , icons_plusBlue_svg = Asset ""
    , icons_quickWrite_svg = Asset ""
    , icons_searchGray_svg = Asset ""
    , icons_xBlue_svg = Asset ""
    , key = ""
    , leaderboard = ""
    , leftArrowBlue_png = Asset ""
    , level1Badge_png = Asset ""
    , level2Badge_png = Asset ""
    , level3Badge_png = Asset ""
    , lock = ""
    , logoRedBlack_svg = Asset ""
    , newspaper = ""
    , openClose = ""
    , performance = ""
    , personBlue_svg = Asset ""
    , practice = ""
    , premiumLock_svg = Asset ""
    , preview = ""
    , quiz = ""
    , seemore = ""
    , share = ""
    , smallCheckmark_png = Asset ""
    , sort = ""
    , sortArrow = ""
    , speedometer = ""
    , squiggly_png = Asset ""
    , startingOffBadge_png = Asset ""
    , teach_assignments_copyWhite_svg = Asset ""
    , twitterBlue_svg = Asset ""
    , unarchiveBlue2x_png = Asset ""
    , writingAssignment = ""
    , writingcycle = ""
    , x = ""
    , xWhite_svg = Asset ""
    }
