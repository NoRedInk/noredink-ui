module Examples.Logo exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Css
import Example exposing (Example)
import Examples.IconExamples as IconExamples
import Html.Styled as Html
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Logo.V1 as Logo
import Nri.Ui.Svg.V1 as Svg


{-| -}
type alias State =
    IconExamples.Settings


{-| -}
type alias Msg =
    IconExamples.Msg


{-| -}
example : Example State Msg
example =
    { name = "Logo"
    , version = 1
    , categories = [ Icons ]
    , keyboardSupport = []
    , state = IconExamples.init
    , update = IconExamples.update
    , subscriptions = \_ -> Sub.none
    , preview =
        Html.div [ css [ Css.marginBottom (Css.px 8) ] ] [ Svg.toHtml Logo.noredink ]
            :: IconExamples.preview
                [ Logo.facebook
                , Logo.twitter
                , Logo.cleverC
                , Logo.googleG
                ]
    , view =
        \settings ->
            let
                viewExampleSection =
                    IconExamples.viewWithCustomStyles settings
            in
            [ IconExamples.viewSettings settings
            , viewExampleSection "NRI"
                [ ( "noredink"
                  , Logo.noredink
                  , [ Css.height (Css.px 25)
                    , Css.width (Css.px 100)
                    , Css.margin (Css.px 4)
                    ]
                  )
                ]
            , viewExampleSection "Social Media"
                [ ( "facebook", Logo.facebook, defaults )
                , ( "twitter", Logo.twitter, defaults )
                ]
            , viewExampleSection "Clever"
                [ ( "clever"
                  , Logo.clever
                  , [ Css.height (Css.px 25)
                    , Css.width (Css.px 100)
                    , Css.margin (Css.px 4)
                    , Css.color Colors.azure
                    ]
                  )
                , ( "cleverC", Logo.cleverC, defaults )
                , ( "cleverLibrary"
                  , Logo.cleverLibrary
                  , [ Css.height (Css.px 25)
                    , Css.width (Css.px 100)
                    , Css.margin (Css.px 4)
                    ]
                  )
                ]
            , viewExampleSection "Google"
                [ ( "googleClassroom"
                  , Logo.googleClassroom
                  , defaults
                  )
                , ( "googleG", Logo.googleG, defaults )
                ]
            , viewExampleSection "LMS"
                [ ( "canvas"
                  , Logo.canvas
                  , [ Css.height (Css.px 25)
                    , Css.width (Css.px 100)
                    , Css.margin (Css.px 4)
                    ]
                  )
                , ( "canvasCircle"
                  , Logo.canvasCircle
                  , [ Css.height (Css.px 25)
                    , Css.width (Css.px 25)
                    , Css.margin (Css.px 4)
                    ]
                  )
                , ( "schoology"
                  , Logo.schoology
                  , [ Css.height (Css.px 25)
                    , Css.width (Css.px 100)
                    , Css.margin (Css.px 4)
                    ]
                  )
                , ( "schoologyCircle"
                  , Logo.schoologyCircle
                  , [ Css.height (Css.px 25)
                    , Css.width (Css.px 25)
                    , Css.margin (Css.px 4)
                    ]
                  )
                ]
            ]
    }


defaults : List Css.Style
defaults =
    [ Css.height (Css.px 25)
    , Css.width (Css.px 25)
    , Css.margin (Css.px 4)
    , Css.color Colors.azure
    ]
