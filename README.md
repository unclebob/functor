# functor
Have you ever written a clojure function and then wanted to refactor it by extracting it into parts?
Did you find that experience frustring because `letfn` is ugly and nesting `lets` is evey uglier?
Me too.  In fact that's my biggest complaint about Clojure.

The `functor` macro is an experiment to see if refactorings like that can be made easier by providing
an Algol-like block structure.  This will allow you to create sub-functions that have access to all
the local variables used by the parent function. 

