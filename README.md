# Important

The src files have been updated to use elm-version 0.17

I am in the process of migrating the tests from elm-version 0.16 to 0.17, so
they are not in a working state for 0.17.

# MultiwayTreeZipper

This library was created with the goal of allowing a multiway tree to be
navigated and updated.

I ran into the problem when designing a tree UI control to use with The Elm
Architecture. I realized that to keep track of the state in the model for things
like whether a node is selected or expanded, I needed a way to update specific
nodes through actions.

There will be more examples to come, but this is meant as a library to solve a
piece of the puzzle, which will be used with others to create a Tree UI control

# Useful commands
    npm install elm -g
    npm install elm-test -g
    elm-test TestRunner.elm -c elm-make.cmd

The commands do the following:
* Install elm
* Install elm-test
* Run elm-test pointing at elm-make.cmd instead of just elm-make because it doesn't work for windows.
