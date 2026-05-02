module Logic.App.Patterns.HexFlow exposing (..)

-- Patterns from HexFlow (https://github.com/YukkuriC/HexFlow)
-- Adds several new patterns for better control for spell evaluations
-- Most are meta/control-flow patterns; in simulation they use noAction

import Array exposing (Array)
import Logic.App.Patterns.OperatorUtils exposing (..)
import Logic.App.Types exposing (ActionResult, CastingContext, Iota(..))


-- pure_map: Thoth-like iteration (meta-pattern)
pureMap : Array Iota -> CastingContext -> ActionResult
pureMap stack ctx =
    noAction stack ctx


-- pure_reduce: fold-like reduction (meta-pattern)
pureReduce : Array Iota -> CastingContext -> ActionResult
pureReduce stack ctx =
    noAction stack ctx


-- for_range/cube: 3D range loop
forRangeCube : Array Iota -> CastingContext -> ActionResult
forRangeCube stack ctx =
    noAction stack ctx


-- for_range/cube/pure: 3D range loop (pure)
forRangeCubePure : Array Iota -> CastingContext -> ActionResult
forRangeCubePure stack ctx =
    noAction stack ctx


-- for_range/line: 1D range loop
forRangeLine : Array Iota -> CastingContext -> ActionResult
forRangeLine stack ctx =
    noAction stack ctx


-- for_range/line/pure: 1D range loop (pure)
forRangeLinePure : Array Iota -> CastingContext -> ActionResult
forRangeLinePure stack ctx =
    noAction stack ctx


-- for_range/floodfill: flood-fill range loop
forRangeFloodfill : Array Iota -> CastingContext -> ActionResult
forRangeFloodfill stack ctx =
    noAction stack ctx


-- for_range/floodfill/pure: flood-fill range loop (pure)
forRangeFloodfillPure : Array Iota -> CastingContext -> ActionResult
forRangeFloodfillPure stack ctx =
    noAction stack ctx


-- build_nested: build nested list from stack
buildNested : Array Iota -> CastingContext -> ActionResult
buildNested stack ctx =
    noAction stack ctx


-- nested_modify: modify nested list element
nestedModify : Array Iota -> CastingContext -> ActionResult
nestedModify stack ctx =
    noAction stack ctx


-- mass_rotate: rotate many items on stack
massRotate : Array Iota -> CastingContext -> ActionResult
massRotate stack ctx =
    noAction stack ctx


-- weak_escape: weaker version of Consideration
weakEscape : Array Iota -> CastingContext -> ActionResult
weakEscape stack ctx =
    noAction stack ctx


-- call_stack: call a pattern-iota as a function
callStack : Array Iota -> CastingContext -> ActionResult
callStack stack ctx =
    noAction stack ctx


noAction : Array Iota -> CastingContext -> ActionResult
noAction stack ctx =
    { stack = stack, ctx = ctx, success = True }
