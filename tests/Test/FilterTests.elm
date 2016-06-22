module Test.FilterTests exposing (..)

import ElmTest exposing (..)
import MultiwayTree exposing (Tree(..))
import MultiwayTreeZipper exposing (..)
import Test.SampleData
    exposing
        ( noChildTree
        , singleChildTree
        , multiChildTree
        , deepTree
        , noChildRecord
        , interestingTree
        )


tests : Test
tests =
    suite "Filter"
        [ test "Filtering a Tree with a predicate that always returns true returns the same tree"
            <| assertEqual (Just interestingTree)
                (MultiwayTree.filter (\_ -> True) interestingTree)
        , test "Filtering a Tree with a predicate returns a filtered Tree"
            <| assertEqual (Just multiChildTree)
                (MultiwayTree.filter (\e -> e < "e") interestingTree)
        , test "If a subtree contains an element which would evaluate the predicate to True it is still not in the result Tree if the parent datum evaluates to false"
            <| assertEqual Nothing
                (MultiwayTree.filter (\e -> e == "k") interestingTree)
          -- , test "If a predicate evaluates to False for a Node but True for one of it's children then the Node will remain in the Tree"
          --     <| assertEqual
          --         (Just
          --             (Tree "a"
          --                 [ Tree "b"
          --                     [ Tree "e"
          --                         [ Tree "k" [] ]
          --                     ]
          --                 ]
          --             )
          --         )
          --         (MultiwayTree.filter (\e -> e == "k") interestingTree)
        ]
