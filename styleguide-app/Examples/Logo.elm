module Examples.Logo exposing (example)

{-|

@docs example, styles

-}

import Category exposing (Category(..))
import Css
import Example exposing (Example)
import Examples.IconExamples as IconExamples
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Logo.V1 as Logo


{-| -}
example : Example () ()
example =
    { name = "Nri.Ui.Logo.V1"
    , categories = List.singleton Icons
    , state = ()
    , update = \_ state -> ( state, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , view =
        \_ ->
            [ IconExamples.viewWithCustomStyles "NRI"
                [ ( "noredink"
                  , Logo.noredink
                  , [ Css.height (Css.px 25)
                    , Css.width (Css.px 100)
                    , Css.margin (Css.px 4)
                    ]
                  )
                ]
            , IconExamples.viewWithCustomStyles "Social Media & SSO"
                [ ( "facebook"
                  , Logo.facebook
                  , defaults
                  )
                , ( "twitter", Logo.twitter, defaults )
                , ( "clever"
                  , Logo.clever
                  , [ Css.height (Css.px 25)
                    , Css.width (Css.px 100)
                    , Css.margin (Css.px 4)
                    , Css.color Colors.azure
                    ]
                  )
                , ( "google classroom"
                  , Logo.googleClassroom
                  , defaults
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
