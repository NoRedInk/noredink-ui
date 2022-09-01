module Nri.Ui.AnimatedIcon.V1 exposing (mobileOpenClose)

import Css
import Css.Animations
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.MediaQuery.V1 as MediaQuery
import Nri.Ui.Svg.V1
import Svg.Styled as Svg
import Svg.Styled.Attributes as Attributes


mobileOpenClose : Bool -> Nri.Ui.Svg.V1.Svg
mobileOpenClose isOpen =
    let
        line ( x_, y ) asX =
            Svg.rect
                [ Attributes.x (String.fromFloat x_)
                , Attributes.y (String.fromFloat y)
                , Attributes.width "25"
                , Attributes.height "5"
                , Attributes.rx "2.5"
                , Attributes.css
                    [ Css.property "transition" "transform 300ms"
                    , if isOpen then
                        Css.transforms asX

                      else
                        Css.batch []
                    ]
                ]
                []
    in
    Nri.Ui.Svg.V1.init "0 0 25 27"
        (List.map2 identity
            [ line ( 0, 0 ), line ( 0, 10 ), line ( 0, 20 ) ]
            [ [ Css.rotate (Css.deg 45)
              , Css.translate3d (Css.px 3) (Css.px -1) Css.zero
              , Css.scaleX 1.1
              ]
            , [ Css.translateX (Css.px 4), Css.scaleX 0 ]
            , [ Css.translate3d (Css.px -15) (Css.px 7.5) Css.zero
              , Css.rotate (Css.deg -45)
              , Css.scaleX 1.1
              ]
            ]
        )
