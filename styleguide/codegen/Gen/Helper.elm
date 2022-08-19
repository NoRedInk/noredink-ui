module Gen.Helper exposing (add5, call_, moduleName_, values_)

{-| 
@docs moduleName_, add5, call_, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Helper" ]


{-| {-|
-}

add5: Int -> Int
-}
add5 : Int -> Elm.Expression
add5 add5Arg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Helper" ]
            , name = "add5"
            , annotation = Just (Type.function [ Type.int ] Type.int)
            }
        )
        [ Elm.int add5Arg ]


call_ : { add5 : Elm.Expression -> Elm.Expression }
call_ =
    { add5 =
        \add5Arg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Helper" ]
                    , name = "add5"
                    , annotation = Just (Type.function [ Type.int ] Type.int)
                    }
                )
                [ add5Arg ]
    }


values_ : { add5 : Elm.Expression }
values_ =
    { add5 =
        Elm.value
            { importFrom = [ "Helper" ]
            , name = "add5"
            , annotation = Just (Type.function [ Type.int ] Type.int)
            }
    }


