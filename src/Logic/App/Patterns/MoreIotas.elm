module Logic.App.Patterns.MoreIotas exposing (..)

-- Patterns from MoreIotas (https://github.com/FallingColors/MoreIotas)
-- Adds new Iota types: String, Matrix, IotaType, EntityType, ItemType, ItemStack
-- and patterns to manipulate them

import Array exposing (Array)
import Logic.App.Patterns.OperatorUtils exposing (..)
import Logic.App.Types exposing (ActionResult, CastingContext, Iota(..), Mishap(..))


-- ============================================================
-- String patterns
-- ============================================================

stringEmpty : Array Iota -> CastingContext -> ActionResult
stringEmpty stack ctx =
    { stack = stack, ctx = ctx, success = True }


stringSpace : Array Iota -> CastingContext -> ActionResult
stringSpace stack ctx =
    { stack = stack, ctx = ctx, success = True }


stringComma : Array Iota -> CastingContext -> ActionResult
stringComma stack ctx =
    { stack = stack, ctx = ctx, success = True }


stringNewline : Array Iota -> CastingContext -> ActionResult
stringNewline stack ctx =
    { stack = stack, ctx = ctx, success = True }


stringBlockGet : Array Iota -> CastingContext -> ActionResult
stringBlockGet stack ctx =
    spell1Input stack ctx getVector


stringBlockSet : Array Iota -> CastingContext -> ActionResult
stringBlockSet stack ctx =
    spell2Inputs stack ctx getVector getMString


stringChatCaster : Array Iota -> CastingContext -> ActionResult
stringChatCaster stack ctx =
    spellNoInput stack ctx


stringChatAll : Array Iota -> CastingContext -> ActionResult
stringChatAll stack ctx =
    spellNoInput stack ctx


stringChatPrefixGet : Array Iota -> CastingContext -> ActionResult
stringChatPrefixGet stack ctx =
    spellNoInput stack ctx


stringChatPrefixSet : Array Iota -> CastingContext -> ActionResult
stringChatPrefixSet stack ctx =
    spell1Input stack ctx getMString


stringIota : Array Iota -> CastingContext -> ActionResult
stringIota stack ctx =
    spell1Input stack ctx getAny


stringAction : Array Iota -> CastingContext -> ActionResult
stringAction stack ctx =
    spell1Input stack ctx getAny


stringNameGet : Array Iota -> CastingContext -> ActionResult
stringNameGet stack ctx =
    spell1Input stack ctx getEntity


stringNameSet : Array Iota -> CastingContext -> ActionResult
stringNameSet stack ctx =
    spell2Inputs stack ctx getEntity getMString


stringSplit : Array Iota -> CastingContext -> ActionResult
stringSplit stack ctx =
    spell2Inputs stack ctx getMString getMString


stringParse : Array Iota -> CastingContext -> ActionResult
stringParse stack ctx =
    spell1Input stack ctx getMString


stringCase : Array Iota -> CastingContext -> ActionResult
stringCase stack ctx =
    spell1Input stack ctx getMString


-- ============================================================
-- Matrix patterns
-- ============================================================

matrixMake : Array Iota -> CastingContext -> ActionResult
matrixMake stack ctx =
    spell1Input stack ctx getIotaList


matrixUnmake : Array Iota -> CastingContext -> ActionResult
matrixUnmake stack ctx =
    spell1Input stack ctx getMMatrix


matrixIdentity : Array Iota -> CastingContext -> ActionResult
matrixIdentity stack ctx =
    spell1Input stack ctx getNumber


matrixZero : Array Iota -> CastingContext -> ActionResult
matrixZero stack ctx =
    spell1Input stack ctx getNumber


matrixRotation : Array Iota -> CastingContext -> ActionResult
matrixRotation stack ctx =
    spell2Inputs stack ctx getNumber getVector


matrixInverse : Array Iota -> CastingContext -> ActionResult
matrixInverse stack ctx =
    spell1Input stack ctx getMMatrix


matrixDeterminant : Array Iota -> CastingContext -> ActionResult
matrixDeterminant stack ctx =
    spell1Input stack ctx getMMatrix


-- ============================================================
-- Type patterns (EntityType, IotaType, ItemType)
-- ============================================================

typeEntity : Array Iota -> CastingContext -> ActionResult
typeEntity stack ctx =
    spell1Input stack ctx getEntity


typeIota : Array Iota -> CastingContext -> ActionResult
typeIota stack ctx =
    spell1Input stack ctx getAny


typeItemHeld : Array Iota -> CastingContext -> ActionResult
typeItemHeld stack ctx =
    spellNoInput stack ctx


getEntityType : Array Iota -> CastingContext -> ActionResult
getEntityType stack ctx =
    spell1Input stack ctx getVector


zoneEntityType : Array Iota -> CastingContext -> ActionResult
zoneEntityType stack ctx =
    spell2Inputs stack ctx getVector getMEntityType


zoneEntityNotType : Array Iota -> CastingContext -> ActionResult
zoneEntityNotType stack ctx =
    spell2Inputs stack ctx getVector getMEntityType


-- ============================================================
-- Item Stack patterns (MoreIotas version)
-- ============================================================

itemGetMainHand : Array Iota -> CastingContext -> ActionResult
itemGetMainHand stack ctx =
    spellNoInput stack ctx


itemGetOffHand : Array Iota -> CastingContext -> ActionResult
itemGetOffHand stack ctx =
    spellNoInput stack ctx


itemGetInventoryStacks : Array Iota -> CastingContext -> ActionResult
itemGetInventoryStacks stack ctx =
    spell1Input stack ctx getEntity


itemGetInventoryItems : Array Iota -> CastingContext -> ActionResult
itemGetInventoryItems stack ctx =
    spell1Input stack ctx getEntity
