module CommonControls exposing (httpError)

import Debug.Control as Control exposing (Control)
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
