module Spec.Nri.Ui.Html.Attributes exposing (spec)

import Expect
import Nri.Ui.Html.Attributes.V2 as Attributes
import Test exposing (..)


spec : Test
spec =
    describe "Nri.Ui.Html.Attributes"
        [ describe "safeId transforms strings as expected"
            [ test "with an apostrophe and multiple spaces" <|
                \() ->
                    Attributes.safeId "Enable text-to-speech for  \t     Angela's account"
                        |> Expect.equal "enable-text-to-speech-for-angela-s-account"
            , test "lotsa hyphens and dashes leading with a non-letter" <|
                \() ->
                    Attributes.safeId "--__--hellO----_______---HOw----___---____--ArE------___You___--__--__Today"
                        |> Expect.equal "id---__--hello----_______---how----___---____--are------___you___--__--__today"
            ]
        , describe "safeIdWithPrefix"
            [ test "test everything at once" <|
                \() ->
                    Attributes.safeIdWithPrefix
                        "000321 ¡¡VERY!! unsafe  "
                        "0--__--hellO----___!?[]____---HOw----___---____--ArE------___You___--__--__Today?"
                        |> Expect.equal "id-000321-very-unsafe--0--__--hello----___-____---how----___---____--are------___you___--__--__today-"
            , test "test everything at once except the prefix" <|
                \() ->
                    Attributes.safeIdWithPrefix
                        "safe_000321 ¡¡VERY!! unsafe  "
                        "0--__--hellO----___!?[]____---HOw----___---____--ArE------___You___--__--__Today?"
                        |> Expect.equal "safe_000321-very-unsafe--0--__--hello----___-____---how----___---____--are------___you___--__--__today-"
            ]
        ]
