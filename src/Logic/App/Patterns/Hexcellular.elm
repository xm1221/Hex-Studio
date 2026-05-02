module Logic.App.Patterns.Hexcellular exposing (..)

-- Patterns from hexcellular (https://github.com/miyucomics/hexcellular)
-- A Hexcasting addon about globally accessible key-value Properties

import Array exposing (Array)
import Logic.App.Patterns.OperatorUtils exposing (..)
import Logic.App.Types exposing (ActionResult, CastingContext, Iota(..), Mishap(..))


-- create_property: creates a new Property with a unique key
-- In simulation, generates a placeholder Property key
createProperty : Array Iota -> CastingContext -> ActionResult
createProperty stack ctx =
    let
        action _ =
            ( Array.repeat 1 (Property "new_property"), ctx )
    in
    actionNoInput stack ctx action


-- observe_property: reads the current value of a Property
-- In simulation, returns Null (user overrides via outputOptions)
observeProperty : Array Iota -> CastingContext -> ActionResult
observeProperty stack ctx =
    spell1Input stack ctx getProperty


-- set_property: sets a Property to a new value
-- Consumes Property + Iota, no return value
setProperty : Array Iota -> CastingContext -> ActionResult
setProperty stack ctx =
    spell2Inputs stack ctx getProperty getAny


-- readonly_property: creates a read-only copy of a Property
-- Takes Property, returns a new Property reference
readonlyProperty : Array Iota -> CastingContext -> ActionResult
readonlyProperty stack ctx =
    let
        action iota _ =
            case iota of
                Property key ->
                    ( Array.repeat 1 (Property (key ++ "_readonly")), ctx )

                _ ->
                    ( Array.repeat 1 (Garbage IncorrectIota), ctx )
    in
    action1Input stack ctx getProperty action
