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
    elm-test Tests.elm --output tests.js
    node tests.js
    
NOTE: elm-test Tests.elm -c elm-make.cmd might have to be used if running on Windows.
