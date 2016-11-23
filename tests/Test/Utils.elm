module Test.Utils exposing (..)


compatibleMaybeAndthen : Maybe a -> (a -> Maybe b) -> Maybe b
compatibleMaybeAndthen maybe callback =
    case maybe of
        Just value ->
            callback value

        Nothing ->
            Nothing


(&>) =
    compatibleMaybeAndthen
