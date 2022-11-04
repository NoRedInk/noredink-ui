module Spec.Nri.Ui.Util exposing (spec)

import Expect
import Nri.Ui.Util as Util
import Test exposing (..)


spec : Test
spec =
    describe "Nri.Ui.Util"
        [ describe "safeId transforms strings as expected"
            [ test "with a comma" <|
                \() ->
                    Util.safeId "Enable text-to-speech for Angela's account"
                        |> Expect.equal "enable-text-to-speech-for-angela-s-account"
            , test "removes leading non alpha characters" <|
                \() ->
                    Util.safeId "#@!!232now we are in business__--__"
                        |> Expect.equal "now-we-are-in-business"
            , test "removes trailing non alphanum characters" <|
                \() ->
                    Util.safeId "#@!!232now we are in business__--__123!!@&*%^ @"
                        |> Expect.equal "now-we-are-in-business-123"
            , test "hard mode" <|
                \() ->
                    Util.safeId "!@#21something else interesting321$ ... hi"
                        |> Expect.equal "something-else-interesting321-hi"
            , test "with capital letters" <|
                \() ->
                    Util.safeId "1232!@#%#@JFEKLfds-----SFJK3@#@jj23FDS........''''\"\"***"
                        |> Expect.equal "jfeklfds-sfjk3-jj23fds"
            , test "lotsa hyphens and dashes" <|
                \() ->
                    Util.safeId "--__--hellO----_______---HOw----___---____--ArE------___You___--__--__Today"
                        |> Expect.equal "hello-how-are-you-today"
            ]
        , describe "safeIdWithPrefix"
            [ test "test everything at once" <|
                \() ->
                    Util.safeIdWithPrefix
                        "000321 ¡¡VERY!! unsafe  "
                        "0--__--hellO----___!?[]____---HOw----___---____--ArE------___You___--__--__Today?"
                        |> Expect.equal "000321-very-unsafe-0-hello-how-are-you-today"
            ]
        ]
