module Nri.Ui.MediaQuery.V2 exposing (..)

import Css exposing (Style, px)
import Css.Media exposing (MediaQuery, maxWidth, minWidth, only, screen, withMedia)
import Maybe.Extra as Maybe


type Attribute
    = Attribute (MediaQuery -> MediaQuery)


type alias MediaQuery =
    { mobile : ( Maybe (List Style), Maybe (List Style) )
    , quizEngineMobile : ( Maybe (List Style), Maybe (List Style) )
    , narrowMobile : ( Maybe (List Style), Maybe (List Style) )
    }


mobile : List Style -> Attribute
mobile s =
    Attribute (\q -> { q | mobile = ( Just s, Tuple.second q.mobile ) })


notMobile : List Style -> Attribute
notMobile s =
    Attribute (\q -> { q | mobile = ( Tuple.first q.mobile, Just s ) })


quizEngineMobile : List Style -> Attribute
quizEngineMobile s =
    Attribute (\q -> { q | quizEngineMobile = ( Just s, Tuple.second q.quizEngineMobile ) })


notQuizEngineMobile : List Style -> Attribute
notQuizEngineMobile s =
    Attribute (\b -> { b | quizEngineMobile = ( Tuple.first b.quizEngineMobile, Just s ) })


narrowMobile : List Style -> Attribute
narrowMobile s =
    Attribute (\b -> { b | narrowMobile = ( Just s, Tuple.second b.narrowMobile ) })


notNarrowMobile : List Style -> Attribute
notNarrowMobile s =
    Attribute (\b -> { b | narrowMobile = ( Tuple.first b.narrowMobile, Just s ) })


styles : List Attribute -> List Style
styles attributes =
    let
        config =
            List.foldl (\(Attribute f) -> f)
                { mobile = ( Nothing, Nothing )
                , quizEngineMobile = ( Nothing, Nothing )
                , narrowMobile = ( Nothing, Nothing )
                }
                attributes

        mkStyle rule size =
            Maybe.map (withMedia [ only screen [ rule (px size) ] ])

        prepend =
            Maybe.cons

        append maybeItem list =
            case maybeItem of
                Just item ->
                    list ++ [ item ]

                Nothing ->
                    list

        addBreakpointStyles ( size, getStyles ) =
            let
                ( lteStyles, gtStyles ) =
                    getStyles config
            in
            prepend (mkStyle maxWidth size lteStyles) >> append (mkStyle minWidth (size + 1) gtStyles)
    in
    []
        |> addBreakpointStyles ( 1000, .mobile )
        |> addBreakpointStyles ( 750, .quizEngineMobile )
        |> addBreakpointStyles ( 500, .narrowMobile )
