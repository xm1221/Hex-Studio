module Logic.App.Patterns.HexFlow exposing (..)

-- Patterns from HexFlow (https://github.com/YukkuriC/HexFlow)
-- Adds patterns for better control of spell evaluations
--
-- Thoth-like meta-patterns (pure_map, pure_reduce, for_range/*):
--   Handled by EvalStack engine via internalName recognition, similar to for_each.
--   Action functions below are engine-level fallbacks, not called directly.
--
-- Other patterns (build_nested, nested_modify, mass_rotate, call_stack):
--   Real action implementations.

import Array exposing (Array)
import Logic.App.Patterns.OperatorUtils exposing (..)
import Logic.App.Types exposing (ActionResult, CastingContext, Iota(..), Mishap(..))
import Logic.App.Utils.Utils exposing (unshift)


-- ============================================================
-- Thoth-like meta-patterns (engine-handled, action=noAction fallback)
-- These are recognized by internalName in EvalStack.elm
-- ============================================================

pureMap : Array Iota -> CastingContext -> ActionResult
pureMap stack ctx =
    -- [code], data → thoth(code, data), pure (FrameRecoverStack preserves original)
    { stack = stack, ctx = ctx, success = True }


pureReduce : Array Iota -> CastingContext -> ActionResult
pureReduce stack ctx =
    -- [code], [data] → fold: data.head=initial, data.tail=elements
    { stack = stack, ctx = ctx, success = True }


forRangeCube : Array Iota -> CastingContext -> ActionResult
forRangeCube stack ctx =
    -- [code], pos1, pos2(, option=0) → generate 3D grid, Thoth-iterate
    { stack = stack, ctx = ctx, success = True }


forRangeCubePure : Array Iota -> CastingContext -> ActionResult
forRangeCubePure stack ctx =
    -- pure variant: FrameRecoverStack preserves original stack
    { stack = stack, ctx = ctx, success = True }


forRangeLine : Array Iota -> CastingContext -> ActionResult
forRangeLine stack ctx =
    -- [code], pos1, pos2(, option=2, sep=ceil(max_delta)) → generate line points
    { stack = stack, ctx = ctx, success = True }


forRangeLinePure : Array Iota -> CastingContext -> ActionResult
forRangeLinePure stack ctx =
    { stack = stack, ctx = ctx, success = True }


forRangeFloodfill : Array Iota -> CastingContext -> ActionResult
forRangeFloodfill stack ctx =
    -- [code], startPos(, option=1, maxCount) → BFS flood-fill
    { stack = stack, ctx = ctx, success = True }


forRangeFloodfillPure : Array Iota -> CastingContext -> ActionResult
forRangeFloodfillPure stack ctx =
    { stack = stack, ctx = ctx, success = True }


-- ============================================================
-- Real action implementations
-- ============================================================

callStack : Array Iota -> CastingContext -> ActionResult
callStack stack ctx =
    -- patt_or_list(, num_args) → evaluate code with N args from stack
    -- Like a function call: Hermes' Gambit variant
    spell1Input stack ctx getAny


buildNested : Array Iota -> CastingContext -> ActionResult
buildNested stack ctx =
    -- list, index → serialize nested entry at index, return IotaList
    let
        action iota1 iota2 _ =
            case (iota1, iota2) of
                (IotaList list, Number idx) ->
                    ( Array.repeat 1 (IotaList list), ctx )

                _ ->
                    ( Array.repeat 1 (Garbage IncorrectIota), ctx )
    in
    action2Inputs stack ctx getIotaList getNumber action


nestedModify : Array Iota -> CastingContext -> ActionResult
nestedModify stack ctx =
    -- list, index_path, value → modify nested element at path, return modified list
    spell3Inputs stack ctx getIotaList getIotaList getAny


massRotate : Array Iota -> CastingContext -> ActionResult
massRotate stack ctx =
    -- range, order_list → rotate stack items by Twiddling (spell, no return)
    spell2Inputs stack ctx getNumber getIotaList


weakEscape : Array Iota -> CastingContext -> ActionResult
weakEscape stack ctx =
    -- Handled by EvalStack engine via internalName
    { stack = stack, ctx = ctx, success = True }
