# elm-interpreter

`elm-interpreter` is an intepreter for Elm, in Elm.

The key function is `Eval.eval : String -> Result Error Value` that takes as input Elm code and executes it.

# about this fork
In this fork, elm-interpreter has been modified to manage additional datatypes in the calculation environmenta as required by elm-foldbook, a multi-dim spreadsheet experiment

# Testing

- Use `elm-test`.
- To make it faster, change the number in `test/Utiles.elm#slowTest`.
