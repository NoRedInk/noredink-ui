module CommonControls exposing (exampleHtml, httpError, quickBrownFox, romeoAndJulietQuotation)

import Debug.Control as Control exposing (Control)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Http


httpError : Control Http.Error
httpError =
    Control.choice
        [ ( "Bad Url", Control.value (Http.BadUrl "/request-url") )
        , ( "Timeout", Control.value Http.Timeout )
        , ( "Network Error", Control.value Http.NetworkError )
        , ( "Bad Status: 401", Control.value (Http.BadStatus 401) )
        , ( "Bad Status: 404", Control.value (Http.BadStatus 404) )
        , ( "Bad Status: ???", Control.value (Http.BadStatus 500) )
        , ( "Bad Body (often, a JSON decoding problem)"
          , Control.value
                (Http.BadBody
                    """
                        The Json.Decode.oneOf at json.draft failed in the following 2 ways:



                        (1) Problem with the given value:

                            null

                            Expecting an OBJECT with a field named `content`



                        (2) Problem with the given value:

                            null

                            Expecting an OBJECT with a field named `code`
                        """
                )
          )
        ]


quickBrownFox : String
quickBrownFox =
    "The quick brown fox jumps over the lazy dog."


romeoAndJulietQuotation : String
romeoAndJulietQuotation =
    """
        Two households, both alike in dignity,
        In fair Verona, where we lay our scene,
        From ancient grudge break to new mutiny,
        Where civil blood makes civil hands unclean.
        From forth the fatal loins of these two foes
        A pair of star-cross’d lovers take their life;
        Whose misadventured piteous overthrows
        Do with their death bury their parents’ strife.
        The fearful passage of their death-mark’d love,
        And the continuance of their parents’ rage,
        Which, but their children’s end, nought could remove,
        Is now the two hours’ traffic of our stage;
        The which if you with patient ears attend,
        What here shall miss, our toil shall strive to mend.
    """


exampleHtml : List (Html msg)
exampleHtml =
    [ Html.text "This is a "
    , Html.strong [] [ Html.text "bolded phrase" ]
    , Html.text ". "
    , Html.a
        [ Attributes.href "http://www.noredink.com"
        , Attributes.target "_blank"
        ]
        [ Html.text quickBrownFox ]
    , Html.text " When I stepped out, into the bright sunlight from the darkness of the movie house, I had only two things on my mind: Paul Newman, and a ride home."
    ]
