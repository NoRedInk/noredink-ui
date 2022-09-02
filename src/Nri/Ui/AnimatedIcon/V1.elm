module Nri.Ui.AnimatedIcon.V1 exposing (mobileOpenClose)

{-|

@docs mobileOpenClose

-}

import Css
import Css.Animations
import Nri.Ui.MediaQuery.V1 as MediaQuery
import Nri.Ui.Svg.V1
import Svg.Styled as Svg
import Svg.Styled.Attributes as Attributes


{-| An icon that animates between a "hamburger" and an "x".

Pass a bool representing whether the menu/navbar/whatever is open.

-}
mobileOpenClose : Bool -> Nri.Ui.Svg.V1.Svg
mobileOpenClose isOpen =
    let
        line y asX =
            Svg.rect
                [ Attributes.x "0"
                , Attributes.y "0"
                , Attributes.width "22"
                , Attributes.height "3"
                , Attributes.rx "1.5"
                , Attributes.ry "1.5"
                , Attributes.css
                    [ Css.property "transition" "transform 300ms"
                    , Css.property "transform-origin" "11px 11px"
                    , if isOpen then
                        Css.transforms asX

                      else
                        Css.transforms [ Css.translateY (Css.px y) ]
                    ]
                ]
                []
    in
    Nri.Ui.Svg.V1.init "-2 0 26 23"
        [ line 0
            [ Css.rotate (Css.deg 45)
            , Css.scaleX 1.3
            , Css.translateY (Css.pct 40)
            ]
        , line 10 [ Css.translateY (Css.px 10), Css.scaleX 0 ]
        , line 20
            [ Css.rotate (Css.deg -45)
            , Css.scaleX 1.3
            , Css.translateY (Css.pct 40)
            ]
        ]
