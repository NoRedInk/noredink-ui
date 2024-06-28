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
            [ StickerIcon.exclamation.detailed
            , StickerIcon.lightBulb.detailed
            , StickerIcon.questionMark.detailed
            , StickerIcon.heart.detailed
            , StickerIcon.star.detailed
            ]
    , all = all
    }
        |> IconExamples.example


all : List Group
all =
    [ ( "Detailed Icons"
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
    , ( "Simple icons"
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
    ]
