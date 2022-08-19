module Gen.Nri.Ui.Loading.V1 exposing (fadeInPage, moduleName_, page, spinningDots, spinningPencil, values_)

{-| 
@docs moduleName_, fadeInPage, page, spinningPencil, spinningDots, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Loading", "V1" ]


{-| View a full-screen loading page that fades into view.

fadeInPage: Html.Styled.Html msg
-}
fadeInPage : Elm.Expression
fadeInPage =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Loading", "V1" ]
        , name = "fadeInPage"
        , annotation =
            Just (Type.namedWith [ "Html", "Styled" ] "Html" [ Type.var "msg" ])
        }


{-| View a full-screen loading page.

page: Html.Styled.Html msg
-}
page : Elm.Expression
page =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Loading", "V1" ]
        , name = "page"
        , annotation =
            Just (Type.namedWith [ "Html", "Styled" ] "Html" [ Type.var "msg" ])
        }


{-| spinningPencil: Nri.Ui.Svg.V1.Svg -}
spinningPencil : Elm.Expression
spinningPencil =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Loading", "V1" ]
        , name = "spinningPencil"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
        }


{-| spinningDots: Nri.Ui.Svg.V1.Svg -}
spinningDots : Elm.Expression
spinningDots =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Loading", "V1" ]
        , name = "spinningDots"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
        }


values_ :
    { fadeInPage : Elm.Expression
    , page : Elm.Expression
    , spinningPencil : Elm.Expression
    , spinningDots : Elm.Expression
    }
values_ =
    { fadeInPage =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Loading", "V1" ]
            , name = "fadeInPage"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Html"
                        [ Type.var "msg" ]
                    )
            }
    , page =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Loading", "V1" ]
            , name = "page"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Html"
                        [ Type.var "msg" ]
                    )
            }
    , spinningPencil =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Loading", "V1" ]
            , name = "spinningPencil"
            , annotation =
                Just (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
            }
    , spinningDots =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Loading", "V1" ]
            , name = "spinningDots"
            , annotation =
                Just (Type.namedWith [ "Nri", "Ui", "Svg", "V1" ] "Svg" [])
            }
    }


