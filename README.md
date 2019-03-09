# MultiwayTreeZipper

This library was created with the goal of allowing a multiway tree to be
navigated and updated.

I ran into the problem when designing a tree UI control to use with The Elm
Architecture. I realized that to keep track of the state in the model for things
like whether a node is selected or expanded, I needed a way to update specific
nodes through actions.

# Run tests (from a cloned repo)
    npm install -g elm
    npm install -g elm-test
    elm-test
