To-do:
* Pattern matching exaustiveness checking
* Type checker rewrite to get rid of duplicate error messages
* Fix trait checker errors like `Type t5 does not implement trait Foo`
* Check for overlaping impls
* Make functions and tuples usable as type constructors (`a -> b` to `Basics.Func a b`)
* Add autoimports


Autoimports:
```
import Basics exposing (..)
import Option exposing (Option(Some, None))
import List exposing (List((::)))
import Result exposing (Result(Ok, Err))
import String exposing (String)
```
