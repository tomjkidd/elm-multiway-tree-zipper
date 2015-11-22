module Test.MultiwayTreeZipper (tests) where

import ElmTest exposing (..)

import MultiwayTree exposing (Tree (..))
import MultiwayTreeZipper exposing (..)

(&>) = Maybe.andThen

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

tests : Test
tests =
    let navigationTests = suite "Navigation"
          [ test "Navigate to child (only child)"
              <| assertEqual
                (Just ((Tree "b" [] ), [ Context "a" [] [] ]))
                (Just (singleChildTree, [])
                    &> goToChild 0)

          , test "Navigate to child (one of many)"
              <| assertEqual
                (Just
                    ((Tree "c" [] ) ,
                     [ Context "a"
                        [ (Tree "b" []) ]
                        [ (Tree "d" []) ]
                     ])
                )
                (Just (multiChildTree, [])
                    &> goToChild 1)

          , test "Navigate to a child (deep)"
              <| assertEqual
                (Just ((Tree "d" [] ),
                [ Context "c" [] []
                , Context "b" [] []
                , Context "a" [] []
                ]))
                (Just (deepTree, [])
                    &> goToChild 0
                    &> goToChild 0
                    &> goToChild 0)

          , test "Navigate up (single level)"
              <| assertEqual
                (Just ((Tree "a" [ Tree "b" [] ]), [] ))
                (Just (singleChildTree, [])
                    &> goToChild 0
                    &> goUp)

          , test "Navigate up (single level with many children)"
              <| assertEqual
                (Just ((Tree "a" [ Tree "b" [], Tree "c" [], Tree "d" [] ]), [] ))
                (Just (multiChildTree, [])
                    &> goToChild 1
                    &> goUp)

          , test "Navigate up from a child (deep)"
              <| assertEqual
                (Just ((Tree "a" [ Tree "b" [ Tree "c" [ Tree "d" [] ]]]), []))
                (Just (deepTree, [])
                    &> goToChild 0
                    &> goToChild 0
                    &> goToChild 0
                    &> goUp
                    &> goUp
                    &> goUp)

          , test "Navigate beyond the tree (only child)"
              <| assertEqual
                Nothing
                (Just (singleChildTree, [])
                    &> goToChild 0
                    &> goToChild 0)

          , test "Navigate beyond the tree (up past root)"
              <| assertEqual
                Nothing
                (Just (singleChildTree, [])
                    &> goUp)
          ]
    in
        suite "MultiwayTreeZipper"
            [ navigationTests
            ]
