module Spec.Nri.Ui.Util exposing (spec)

import Expect
import Nri.Ui.Util as Util
import Test exposing (..)


spec : Test
spec =
    describe "Nri.Ui.Util"
        [ describe "safeIdString transforms strings as expected"
            [ test "with a comma" <|
                \() ->
                    Util.safeIdString "Enable text-to-speech for Angela's account"
                        |> Expect.equal "enable-text-to-speech-for-angela-s-account"
            , test "removes leading non alpha characters" <|
                \() ->
                    Util.safeIdString "#@!!232now we are in business__--__"
                        |> Expect.equal "now-we-are-in-business"
            , test "removes trailing non alphanum characters" <|
                \() ->
                    Util.safeIdString "#@!!232now we are in business__--__123!!@&*%^ @"
                        |> Expect.equal "now-we-are-in-business-123"
            , test "hard mode" <|
                \() ->
                    Util.safeIdString "!@#21something else interesting321$ ... hi"
                        |> Expect.equal "something-else-interesting321-hi"
            , test "with capital letters" <|
                \() ->
                    Util.safeIdString "1232!@#%#@JFEKLfds-----SFJK3@#@jj23FDS........''''\"\"***"
                        |> Expect.equal "jfeklfds-sfjk3-jj23fds"
            , test "lotsa hyphens and dashes" <|
                \() ->
                    Util.safeIdString "--__--hellO----_______---HOw----___---____--ArE------___You___--__--__Today"
                        |> Expect.equal "hello-how-are-you-today"
            ]
        , describe "removePunctuation"
            [ test "A string with some punctuation" <|
                \() ->
                    Util.removePunctuation
                        "To sleep? Perchance: to dream? “Alas poor ‘Yorick’” but he's not `in` _this_ \"[play]\"."
                        |> Expect.equal "To sleep Perchance to dream Alas poor Yorick but hes not in _this_ play"
            ]
        ]
