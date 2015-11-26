module Test.SampleData where

import MultiwayTree exposing (Tree (..))

interestingTree : Tree String
interestingTree =
    Tree "a"
        [ Tree "b"
            [ Tree "e"
                [ Tree "k" [] ]
            ]
        , Tree "c"
            [ Tree "f" []
            , Tree "g" []
            ]
        , Tree "d"
            [ Tree "h" []
            , Tree "i" []
            , Tree "j" []
            ]
        ]

noChildTree =
    Tree "a" []

noChildRecord =
    Tree { selected = False, expanded = False } []

singleChildTree =
    Tree "a"
        [ Tree "b" [] ]

multiChildTree =
    Tree "a"
        [ Tree "b" []
        , Tree "c" []
        , Tree "d" []
        ]

deepTree =
    Tree "a"
        [ Tree "b"
            [ Tree "c"
                [ Tree "d" [] ]
            ]
        ]
