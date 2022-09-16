module Examples.Logo exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Css
import Example exposing (Example)
import Examples.IconExamples as IconExamples exposing (Group)
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
    { moduleName = "Logo"
    , version = 1
    , label = "NoRedInk"
    , name = "noredink"
    , icon = Logo.noredink
    , renderSvgCode = \name -> "Logo." ++ name
    , preview =
        Html.div [ css [ Css.marginBottom (Css.px 8) ] ] [ Svg.toHtml Logo.noredink ]
            :: IconExamples.preview
                [ Logo.facebook
                , Logo.twitter
                , Logo.cleverC
                , Logo.googleG
                ]
    , all = all
    }
        |> IconExamples.example


all : List Group
all =
    [ ( "NRI"
      , [ ( "noredink"
          , Logo.noredink
          , [ Css.height (Css.px 25)
            , Css.width (Css.px 100)
            , Css.margin (Css.px 4)
            ]
          )
        , ( "noredinkMonochrome"
          , Logo.noredinkMonochrome
          , [ Css.height (Css.px 25)
            , Css.width (Css.px 100)
            , Css.margin (Css.px 4)
            ]
          )
        ]
      )
    , ( "Social Media"
      , [ ( "facebook", Logo.facebook, defaults )
        , ( "twitter", Logo.twitter, defaults )
        ]
      )
    , ( "Google"
      , [ ( "google"
          , Logo.google
          , [ Css.height (Css.px 25)
            , Css.width (Css.px 100)
            , Css.margin (Css.px 4)
            ]
          )
        , ( "googleG", Logo.googleG, defaults )
        ]
      )
    , ( "Google Classroom"
      , [ ( "googleClassroomFull"
          , Logo.googleClassroomFull
          , [ Css.height (Css.px 25)
            , Css.width (Css.px 100)
            , Css.margin (Css.px 4)
            ]
          )
        , ( "googleClassroom"
          , Logo.googleClassroom
          , defaults
          )
        ]
      )
    , ( "Clever library"
      , [ ( "cleverBlue"
          , Logo.cleverBlue
          , [ Css.height (Css.px 25)
            , Css.width (Css.px 100)
            , Css.margin (Css.px 4)
            ]
          )
        , ( "cleverWhite"
          , Logo.cleverWhite
          , [ Css.height (Css.px 25)
            , Css.width (Css.px 100)
            , Css.margin (Css.px 4)
            ]
          )
        ]
      )
    , ( "Clever sync"
      , [ ( "cleverSecureSync"
          , Logo.cleverSecureSync
          , [ Css.height (Css.px 25)
            , Css.width (Css.px 100)
            , Css.margin (Css.px 4)
            , Css.color Colors.azure
            ]
          )
        , ( "cleverC", Logo.cleverC, defaults )
        ]
      )
    , ( "Canvas"
      , [ ( "canvas"
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
        ]
      )
    , ( "Schoology"
      , [ ( "schoology"
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
      )
    ]


defaults : List Css.Style
defaults =
    [ Css.height (Css.px 25)
    , Css.width (Css.px 25)
    , Css.margin (Css.px 4)
    , Css.color Colors.azure
    ]
