module Examples.StickerIcon exposing (Msg, State, example)

import Code
import Css
import Example exposing (Example)
import IconExamples exposing (Group)
import Nri.Ui.StickerIcon.V1 as StickerIcon
import Nri.Ui.Svg.V1


{-| -}
type alias State =
    IconExamples.Settings


{-| -}
type alias Msg =
    IconExamples.Msg


moduleName : String
moduleName =
    "StickerIcon"


{-| -}
example : Example State Msg
example =
    { moduleName = moduleName
    , version = 1
    , label = "Exclamation Detailed"
    , name = "exclamation.detailed"

    -- We need to start off with a blank example image to avoid axe violations for having duplicated ids
    , icon = Nri.Ui.Svg.V1.init "0 0 800 800" []
    , renderSvgCode = \name -> Code.fromModule moduleName name
    , preview =
        IconExamples.previewCustomSize ( Just 50, Just 50 )
            [ StickerIcon.exclamation.expressive
            , StickerIcon.lightBulb.expressive
            , StickerIcon.questionMark.expressive
            , StickerIcon.heart.expressive
            , StickerIcon.star.expressive
            ]
    , all = all
    }
        |> IconExamples.example


all : List Group
all =
    [ ( "Simple icons"
      , [ ( "exclamation.simple"
          , StickerIcon.exclamation.simple
          , [ Css.width (Css.px 100) ]
          )
        , ( "lightBulb.simple"
          , StickerIcon.lightBulb.simple
          , [ Css.width (Css.px 100) ]
          )
        , ( "questionMark.simple"
          , StickerIcon.questionMark.simple
          , [ Css.width (Css.px 100) ]
          )
        , ( "heart.simple"
          , StickerIcon.heart.simple
          , [ Css.width (Css.px 100) ]
          )
        , ( "star.simple"
          , StickerIcon.star.simple
          , [ Css.width (Css.px 100) ]
          )
        ]
      )
    , ( "Detailed Icons"
      , [ ( "exclamation.detailed"
          , StickerIcon.exclamation.detailed
          , [ Css.width (Css.px 100) ]
          )
        , ( "lightBulb.detailed"
          , StickerIcon.lightBulb.detailed
          , [ Css.width (Css.px 100) ]
          )
        , ( "questionMark.detailed"
          , StickerIcon.questionMark.detailed
          , [ Css.width (Css.px 100) ]
          )
        , ( "heart.detailed"
          , StickerIcon.heart.detailed
          , [ Css.width (Css.px 100) ]
          )
        , ( "star.detailed"
          , StickerIcon.star.detailed
          , [ Css.width (Css.px 100) ]
          )
        ]
      )
    , ( "Expressive Icons"
      , [ ( "exclamation.expressive"
          , StickerIcon.exclamation.expressive
          , [ Css.width (Css.px 100) ]
          )
        , ( "lightBulb.expressive"
          , StickerIcon.lightBulb.expressive
          , [ Css.width (Css.px 100) ]
          )
        , ( "questionMark.expressive"
          , StickerIcon.questionMark.expressive
          , [ Css.width (Css.px 100) ]
          )
        , ( "heart.expressive"
          , StickerIcon.heart.expressive
          , [ Css.width (Css.px 100) ]
          )
        , ( "star.expressive"
          , StickerIcon.star.expressive
          , [ Css.width (Css.px 100) ]
          )
        ]
      )
    , ( "Animated Icons"
      , [ ( "exclamation.animated"
          , StickerIcon.exclamation.animated
          , [ Css.width (Css.px 100) ]
          )
        , ( "lightBulb.animated"
          , StickerIcon.lightBulb.animated
          , [ Css.width (Css.px 100) ]
          )
        , ( "questionMark.animated"
          , StickerIcon.questionMark.animated
          , [ Css.width (Css.px 100) ]
          )
        , ( "heart.animated"
          , StickerIcon.heart.animated
          , [ Css.width (Css.px 100) ]
          )
        , ( "star.animated"
          , StickerIcon.star.animated
          , [ Css.width (Css.px 100) ]
          )
        ]
      )
    ]
