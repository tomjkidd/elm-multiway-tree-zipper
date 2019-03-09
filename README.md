# MultiwayTreeZipper

This library was created with the goal of allowing a multiway tree to be
navigated and updated.

I ran into the problem when designing a tree UI control to use with The Elm
Architecture. I realized that to keep track of the state in the model for things
like whether a node is selected or expanded, I needed a way to update specific
nodes through actions.

There will be more examples to come, but this is meant as a library to solve a
piece of the puzzle, which will be used with others to create a Tree UI control

# Run tests (from a cloned repo)
    npm install -g elm
    cd tests
    elm-package install -y
    elm-make Tests.elm --output tests.js
    node tests.js

NOTE: elm-test Tests.elm -c elm-make.cmd might have to be used if running on Windows.

# Looking for Elm 0.19?

I upgraded to the latest version of elm in a separate branch due to changes to both
how dependencies are managed and tests are performed. That code lives in the [elm-0.19-upgrade][1] branch, and has been [deployed to Elm Packages][2] as version `1.10.3`.

The API did not change, however, the custom operator that was demonstrated in docs
and the tests, `(&>)` can no longer be used as of 0.19. I updated both to use the
best concise alternative I could, which uses`|> Maybe.andThen` and partial application.
See the test for an example!

[1]: https://github.com/tomjkidd/elm-multiway-tree-zipper/tree/elm-0.19-upgrade
[2]: https://package.elm-lang.org/packages/tomjkidd/elm-multiway-tree-zipper/1.10.3/
