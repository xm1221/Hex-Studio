module Logic.App.Patterns.Hexpose exposing (..)

-- Patterns from hexpose (https://github.com/miyucomics/hexpose)
-- A Hexcasting addon about getting information about the world
--
-- These patterns query Minecraft world/entity/block/item data.
-- Since Elm runs in a browser without a real Minecraft world, most
-- patterns use spell*Input helpers and rely on outputOptions /
-- selectedOutput for the user to manually set expected values.
--
-- Only patterns that do pure data computation (not world queries)
-- implement actual action logic.

import Array exposing (Array)
import Array.Extra as Array
import Logic.App.Patterns.OperatorUtils exposing (..)
import Logic.App.Types exposing (ActionResult, CastingContext, Iota(..), Mishap(..))
import Logic.App.Utils.Utils exposing (unshift)


-- ============================================================
-- Meta / Enlightenment
-- ============================================================

amEnlightened : Array Iota -> CastingContext -> ActionResult
amEnlightened stack ctx =
    spell1Input stack ctx getEntity


isBrainswept : Array Iota -> CastingContext -> ActionResult
isBrainswept stack ctx =
    spell1Input stack ctx getEntity


-- ============================================================
-- Display iota creation & manipulation
-- ============================================================

createDisplay : Array Iota -> CastingContext -> ActionResult
createDisplay stack ctx =
    let
        action iota _ =
            case iota of
                Display t s ->
                    ( Array.repeat 1 (Display t ""), ctx )

                _ ->
                    ( Array.repeat 1 (Display (getIotaDisplayString iota) ""), ctx )
    in
    action1Input stack ctx getAny action


displayChildren : Array Iota -> CastingContext -> ActionResult
displayChildren stack ctx =
    let
        action iota1 iota2 _ =
            case iota1 of
                Display t s ->
                    ( Array.repeat 1 (Display t s), ctx )

                _ ->
                    ( Array.repeat 1 (Garbage IncorrectIota), ctx )
    in
    action2Inputs stack ctx getDisplay getIotaList action


displayColor : Array Iota -> CastingContext -> ActionResult
displayColor stack ctx =
    let
        action iota1 iota2 _ =
            case iota1 of
                Display t s ->
                    case iota2 of
                        Vector ( r, g, b ) ->
                            let
                                newStyle =
                                    "color:rgb(" ++ String.fromFloat (r * 255) ++ "," ++ String.fromFloat (g * 255) ++ "," ++ String.fromFloat (b * 255) ++ ");" ++ s
                            in
                            ( Array.repeat 1 (Display t newStyle), ctx )

                        Null ->
                            ( Array.repeat 1 (Display t s), ctx )

                        _ ->
                            ( Array.repeat 1 (Garbage IncorrectIota), ctx )

                _ ->
                    ( Array.repeat 1 (Garbage IncorrectIota), ctx )
    in
    action2Inputs stack ctx getDisplay getAny action


displayBoolean : String -> (String -> String) -> Array Iota -> CastingContext -> ActionResult
displayBoolean propName styleFn stack ctx =
    let
        action iota1 iota2 _ =
            case iota1 of
                Display t s ->
                    case iota2 of
                        Boolean b ->
                            let
                                newStyle =
                                    if b then
                                        propName ++ "=true;" ++ s

                                    else
                                        propName ++ "=false;" ++ s
                            in
                            ( Array.repeat 1 (Display t newStyle), ctx )

                        Null ->
                            ( Array.repeat 1 (Display t s), ctx )

                        _ ->
                            ( Array.repeat 1 (Garbage IncorrectIota), ctx )

                _ ->
                    ( Array.repeat 1 (Garbage IncorrectIota), ctx )
    in
    action2Inputs stack ctx getDisplay getAny action


displayBold : Array Iota -> CastingContext -> ActionResult
displayBold = displayBoolean "bold" identity

displayItalics : Array Iota -> CastingContext -> ActionResult
displayItalics = displayBoolean "italic" identity

displayUnderline : Array Iota -> CastingContext -> ActionResult
displayUnderline = displayBoolean "underline" identity

displayStrikethrough : Array Iota -> CastingContext -> ActionResult
displayStrikethrough = displayBoolean "strikethrough" identity

displayObfuscated : Array Iota -> CastingContext -> ActionResult
displayObfuscated = displayBoolean "obfuscated" identity


displayFont : Array Iota -> CastingContext -> ActionResult
displayFont stack ctx =
    let
        action iota1 iota2 _ =
            case iota1 of
                Display t s ->
                    case iota2 of
                        Number n ->
                            ( Array.repeat 1 (Display t ("font=" ++ String.fromFloat n ++ ";" ++ s)), ctx )

                        Null ->
                            ( Array.repeat 1 (Display t s), ctx )

                        _ ->
                            ( Array.repeat 1 (Garbage IncorrectIota), ctx )

                _ ->
                    ( Array.repeat 1 (Garbage IncorrectIota), ctx )
    in
    action2Inputs stack ctx getDisplay getAny action


compareStyle : Array Iota -> CastingContext -> ActionResult
compareStyle stack ctx =
    let
        action iota1 iota2 _ =
            case ( iota1, iota2 ) of
                ( Display _ s1, Display _ s2 ) ->
                    ( Array.repeat 1 (Boolean (s1 == s2)), ctx )

                _ ->
                    ( Array.repeat 1 (Garbage IncorrectIota), ctx )
    in
    action2Inputs stack ctx getDisplay getDisplay action


parseDisplay : Array Iota -> CastingContext -> ActionResult
parseDisplay stack ctx =
    spell1Input stack ctx getDisplay


splitDisplay : Array Iota -> CastingContext -> ActionResult
splitDisplay stack ctx =
    spell2Inputs stack ctx getDisplay getDisplay


disintegrateDisplay : Array Iota -> CastingContext -> ActionResult
disintegrateDisplay stack ctx =
    spell1Input stack ctx getDisplay


-- ============================================================
-- Block state queries
-- ============================================================

isBlockAir : Array Iota -> CastingContext -> ActionResult
isBlockAir stack ctx =
    spell1Input stack ctx getVector


isBlockReplaceable : Array Iota -> CastingContext -> ActionResult
isBlockReplaceable stack ctx =
    spell1Input stack ctx getVector


blockHardness : Array Iota -> CastingContext -> ActionResult
blockHardness stack ctx =
    spell1Input stack ctx getVector


blockBlastResistance : Array Iota -> CastingContext -> ActionResult
blockBlastResistance stack ctx =
    spell1Input stack ctx getVector


blockstateRotation : Array Iota -> CastingContext -> ActionResult
blockstateRotation stack ctx =
    spell1Input stack ctx getVector


blockstateCrop : Array Iota -> CastingContext -> ActionResult
blockstateCrop stack ctx =
    spell1Input stack ctx getVector


getBlockstates : Array Iota -> CastingContext -> ActionResult
getBlockstates stack ctx =
    spell1Input stack ctx getVector


queryBlockstate : Array Iota -> CastingContext -> ActionResult
queryBlockstate stack ctx =
    spell2Inputs stack ctx getVector getIdentifier


blockSlipperiness : Array Iota -> CastingContext -> ActionResult
blockSlipperiness stack ctx =
    spell1Input stack ctx getVector


blockMapColor : Array Iota -> CastingContext -> ActionResult
blockMapColor stack ctx =
    spell1Input stack ctx getVector


-- ============================================================
-- Chat / Messages
-- ============================================================

getMessage : Array Iota -> CastingContext -> ActionResult
getMessage stack ctx =
    spellNoInput stack ctx


getMessageIndexed : Array Iota -> CastingContext -> ActionResult
getMessageIndexed stack ctx =
    spell1Input stack ctx getNumber


-- ============================================================
-- Enchantment queries
-- ============================================================

getEnchantments : Array Iota -> CastingContext -> ActionResult
getEnchantments stack ctx =
    spell1Input stack ctx getItemStack


getEnchantmentStrength : Array Iota -> CastingContext -> ActionResult
getEnchantmentStrength stack ctx =
    spell2Inputs stack ctx getItemStack getIdentifier


enchantmentWeight : Array Iota -> CastingContext -> ActionResult
enchantmentWeight stack ctx =
    spell1Input stack ctx getIdentifier


canItemSupportEnchantment : Array Iota -> CastingContext -> ActionResult
canItemSupportEnchantment stack ctx =
    spell2Inputs stack ctx getItemStack getIdentifier


enchantmentMinLevel : Array Iota -> CastingContext -> ActionResult
enchantmentMinLevel stack ctx =
    spell1Input stack ctx getIdentifier


enchantmentMaxLevel : Array Iota -> CastingContext -> ActionResult
enchantmentMaxLevel stack ctx =
    spell1Input stack ctx getIdentifier


isEnchantmentCursed : Array Iota -> CastingContext -> ActionResult
isEnchantmentCursed stack ctx =
    spell1Input stack ctx getIdentifier


isEnchantmentTreasure : Array Iota -> CastingContext -> ActionResult
isEnchantmentTreasure stack ctx =
    spell1Input stack ctx getIdentifier


-- ============================================================
-- Entity data queries
-- ============================================================

entityWidth : Array Iota -> CastingContext -> ActionResult
entityWidth stack ctx =
    spell1Input stack ctx getEntity


theodolite : Array Iota -> CastingContext -> ActionResult
theodolite stack ctx =
    spell1Input stack ctx getEntity


getHealth : Array Iota -> CastingContext -> ActionResult
getHealth stack ctx =
    spell1Input stack ctx getEntity


getMaxHealth : Array Iota -> CastingContext -> ActionResult
getMaxHealth stack ctx =
    spell1Input stack ctx getEntity


burning : Array Iota -> CastingContext -> ActionResult
burning stack ctx =
    spell1Input stack ctx getEntity


isWet : Array Iota -> CastingContext -> ActionResult
isWet stack ctx =
    spell1Input stack ctx getEntity


getAir : Array Iota -> CastingContext -> ActionResult
getAir stack ctx =
    spell1Input stack ctx getEntity


getMaxAir : Array Iota -> CastingContext -> ActionResult
getMaxAir stack ctx =
    spell1Input stack ctx getEntity


isSleeping : Array Iota -> CastingContext -> ActionResult
isSleeping stack ctx =
    spell1Input stack ctx getEntity


isSprinting : Array Iota -> CastingContext -> ActionResult
isSprinting stack ctx =
    spell1Input stack ctx getEntity


isBaby : Array Iota -> CastingContext -> ActionResult
isBaby stack ctx =
    spell1Input stack ctx getEntity


breedable : Array Iota -> CastingContext -> ActionResult
breedable stack ctx =
    spell1Input stack ctx getEntity


entityVehicle : Array Iota -> CastingContext -> ActionResult
entityVehicle stack ctx =
    spell1Input stack ctx getEntity


entityPassengers : Array Iota -> CastingContext -> ActionResult
entityPassengers stack ctx =
    spell1Input stack ctx getEntity


angryAt : Array Iota -> CastingContext -> ActionResult
angryAt stack ctx =
    spell1Input stack ctx getEntity


angryTime : Array Iota -> CastingContext -> ActionResult
angryTime stack ctx =
    spell1Input stack ctx getEntity


lastAttacker : Array Iota -> CastingContext -> ActionResult
lastAttacker stack ctx =
    spell1Input stack ctx getEntity


lastAttacked : Array Iota -> CastingContext -> ActionResult
lastAttacked stack ctx =
    spell1Input stack ctx getEntity


entityName : Array Iota -> CastingContext -> ActionResult
entityName stack ctx =
    spell1Input stack ctx getEntity


petOwner : Array Iota -> CastingContext -> ActionResult
petOwner stack ctx =
    spell1Input stack ctx getEntity


isMonster : Array Iota -> CastingContext -> ActionResult
isMonster stack ctx =
    spell1Input stack ctx getEntity


shooter : Array Iota -> CastingContext -> ActionResult
shooter stack ctx =
    spell1Input stack ctx getEntity


absorptionHearts : Array Iota -> CastingContext -> ActionResult
absorptionHearts stack ctx =
    spell1Input stack ctx getEntity


-- ============================================================
-- Environment queries
-- ============================================================

envAmbit : Array Iota -> CastingContext -> ActionResult
envAmbit stack ctx =
    spell1Input stack ctx getAny


envStaff : Array Iota -> CastingContext -> ActionResult
envStaff stack ctx =
    spellNoInput stack ctx


envOffhand : Array Iota -> CastingContext -> ActionResult
envOffhand stack ctx =
    spellNoInput stack ctx


envPackagedHex : Array Iota -> CastingContext -> ActionResult
envPackagedHex stack ctx =
    spellNoInput stack ctx


envCircle : Array Iota -> CastingContext -> ActionResult
envCircle stack ctx =
    spellNoInput stack ctx


-- ============================================================
-- Food / Hunger
-- ============================================================

getPlayerHunger : Array Iota -> CastingContext -> ActionResult
getPlayerHunger stack ctx =
    spell1Input stack ctx getEntity


getPlayerSaturation : Array Iota -> CastingContext -> ActionResult
getPlayerSaturation stack ctx =
    spell1Input stack ctx getEntity


getHunger : Array Iota -> CastingContext -> ActionResult
getHunger stack ctx =
    spell1Input stack ctx getIdentifier


getSaturation : Array Iota -> CastingContext -> ActionResult
getSaturation stack ctx =
    spell1Input stack ctx getIdentifier


isMeat : Array Iota -> CastingContext -> ActionResult
isMeat stack ctx =
    spell1Input stack ctx getIdentifier


isSnack : Array Iota -> CastingContext -> ActionResult
isSnack stack ctx =
    spell1Input stack ctx getIdentifier


edible : Array Iota -> CastingContext -> ActionResult
edible stack ctx =
    spell1Input stack ctx getIdentifier


-- ============================================================
-- Identifier operations
-- ============================================================

identify : Array Iota -> CastingContext -> ActionResult
identify stack ctx =
    spell1Input stack ctx getAny


classify : Array Iota -> CastingContext -> ActionResult
classify stack ctx =
    spell1Input stack ctx getAny


-- ============================================================
-- Item stack operations
-- ============================================================

getStack : Array Iota -> CastingContext -> ActionResult
getStack stack ctx =
    spell1Input stack ctx getEntity


createStack : Array Iota -> CastingContext -> ActionResult
createStack stack ctx =
    let
        action iota1 iota2 _ =
            case ( iota1, iota2 ) of
                ( Identifier id, Number count ) ->
                    ( Array.repeat 1 (ItemStack id count), ctx )

                _ ->
                    ( Array.repeat 1 (Garbage IncorrectIota), ctx )
    in
    action2Inputs stack ctx getIdentifier getNumber action


getMainhand : Array Iota -> CastingContext -> ActionResult
getMainhand stack ctx =
    spell1Input stack ctx getEntity


getOffhand : Array Iota -> CastingContext -> ActionResult
getOffhand stack ctx =
    spell1Input stack ctx getEntity


getArmor : Array Iota -> CastingContext -> ActionResult
getArmor stack ctx =
    spell1Input stack ctx getEntity


getEnderChest : Array Iota -> CastingContext -> ActionResult
getEnderChest stack ctx =
    spellNoInput stack ctx


getInventory : Array Iota -> CastingContext -> ActionResult
getInventory stack ctx =
    spell1Input stack ctx getEntity


getBlockInventory : Array Iota -> CastingContext -> ActionResult
getBlockInventory stack ctx =
    spell1Input stack ctx getVector


countStack : Array Iota -> CastingContext -> ActionResult
countStack stack ctx =
    spell1Input stack ctx getItemStack


countMaxStack : Array Iota -> CastingContext -> ActionResult
countMaxStack stack ctx =
    spell1Input stack ctx getIdentifier


damageStack : Array Iota -> CastingContext -> ActionResult
damageStack stack ctx =
    spell1Input stack ctx getItemStack


damageMaxStack : Array Iota -> CastingContext -> ActionResult
damageMaxStack stack ctx =
    spell1Input stack ctx getIdentifier


itemVariant : Array Iota -> CastingContext -> ActionResult
itemVariant stack ctx =
    spell1Input stack ctx getItemStack


itemVariantMax : Array Iota -> CastingContext -> ActionResult
itemVariantMax stack ctx =
    spell1Input stack ctx getItemStack


itemName : Array Iota -> CastingContext -> ActionResult
itemName stack ctx =
    spell1Input stack ctx getItemStack


itemLore : Array Iota -> CastingContext -> ActionResult
itemLore stack ctx =
    spell1Input stack ctx getItemStack


readBook : Array Iota -> CastingContext -> ActionResult
readBook stack ctx =
    spell1Input stack ctx getItemStack


bookSources : Array Iota -> CastingContext -> ActionResult
bookSources stack ctx =
    spell1Input stack ctx getItemStack


itemRarity : Array Iota -> CastingContext -> ActionResult
itemRarity stack ctx =
    spell1Input stack ctx getItemStack


-- ============================================================
-- Media queries
-- ============================================================

envMedia : Array Iota -> CastingContext -> ActionResult
envMedia stack ctx =
    spellNoInput stack ctx


getMedia : Array Iota -> CastingContext -> ActionResult
getMedia stack ctx =
    spell1Input stack ctx getAny


getMaxMedia : Array Iota -> CastingContext -> ActionResult
getMaxMedia stack ctx =
    spell1Input stack ctx getAny


-- ============================================================
-- Miscellaneous entity-related
-- ============================================================

catVariant : Array Iota -> CastingContext -> ActionResult
catVariant stack ctx =
    spell1Input stack ctx getEntity


creeperFuse : Array Iota -> CastingContext -> ActionResult
creeperFuse stack ctx =
    spell1Input stack ctx getEntity


getItemFrameRotation : Array Iota -> CastingContext -> ActionResult
getItemFrameRotation stack ctx =
    spell1Input stack ctx getEntity


setItemFrameRotation : Array Iota -> CastingContext -> ActionResult
setItemFrameRotation stack ctx =
    spell2Inputs stack ctx getEntity getNumber


paintingVariant : Array Iota -> CastingContext -> ActionResult
paintingVariant stack ctx =
    spell1Input stack ctx getEntity


-- ============================================================
-- Status Effect queries
-- ============================================================

getEffectsEntity : Array Iota -> CastingContext -> ActionResult
getEffectsEntity stack ctx =
    spell1Input stack ctx getEntity


getEffectsItem : Array Iota -> CastingContext -> ActionResult
getEffectsItem stack ctx =
    spell1Input stack ctx getItemStack


getEffectCategory : Array Iota -> CastingContext -> ActionResult
getEffectCategory stack ctx =
    spell1Input stack ctx getIdentifier


getEffectAmplifier : Array Iota -> CastingContext -> ActionResult
getEffectAmplifier stack ctx =
    spell2Inputs stack ctx getEntity getIdentifier


getEffectDuration : Array Iota -> CastingContext -> ActionResult
getEffectDuration stack ctx =
    spell2Inputs stack ctx getEntity getIdentifier


-- ============================================================
-- Tag queries
-- ============================================================

blockTags : Array Iota -> CastingContext -> ActionResult
blockTags stack ctx =
    spell1Input stack ctx getVector


entityTags : Array Iota -> CastingContext -> ActionResult
entityTags stack ctx =
    spell1Input stack ctx getEntity


itemTags : Array Iota -> CastingContext -> ActionResult
itemTags stack ctx =
    spell1Input stack ctx getAny


-- ============================================================
-- Villager queries
-- ============================================================

villagerLevel : Array Iota -> CastingContext -> ActionResult
villagerLevel stack ctx =
    spell1Input stack ctx getEntity


villagerProfession : Array Iota -> CastingContext -> ActionResult
villagerProfession stack ctx =
    spell1Input stack ctx getEntity


villagerType : Array Iota -> CastingContext -> ActionResult
villagerType stack ctx =
    spell1Input stack ctx getEntity


biomeToVillager : Array Iota -> CastingContext -> ActionResult
biomeToVillager stack ctx =
    spell1Input stack ctx getIdentifier


-- ============================================================
-- World / Position queries
-- ============================================================

getWeather : Array Iota -> CastingContext -> ActionResult
getWeather stack ctx =
    spellNoInput stack ctx


getLight : Array Iota -> CastingContext -> ActionResult
getLight stack ctx =
    spell1Input stack ctx getVector


getPower : Array Iota -> CastingContext -> ActionResult
getPower stack ctx =
    spell1Input stack ctx getVector


getComparator : Array Iota -> CastingContext -> ActionResult
getComparator stack ctx =
    spell1Input stack ctx getVector


getDay : Array Iota -> CastingContext -> ActionResult
getDay stack ctx =
    spellNoInput stack ctx


getTime : Array Iota -> CastingContext -> ActionResult
getTime stack ctx =
    spellNoInput stack ctx


getBiome : Array Iota -> CastingContext -> ActionResult
getBiome stack ctx =
    spell1Input stack ctx getVector


getDimension : Array Iota -> CastingContext -> ActionResult
getDimension stack ctx =
    spellNoInput stack ctx


getMoon : Array Iota -> CastingContext -> ActionResult
getMoon stack ctx =
    spellNoInput stack ctx


getSlime : Array Iota -> CastingContext -> ActionResult
getSlime stack ctx =
    spell1Input stack ctx getVector


getChunkLoaded : Array Iota -> CastingContext -> ActionResult
getChunkLoaded stack ctx =
    spell1Input stack ctx getVector


getEinstein : Array Iota -> CastingContext -> ActionResult
getEinstein stack ctx =
    spellNoInput stack ctx


-- ============================================================
-- Item renaming (spells that affect world - no real effect)
-- ============================================================

setItemName : Array Iota -> CastingContext -> ActionResult
setItemName stack ctx =
    spell2Inputs stack ctx getItemStack getDisplay


setItemLore : Array Iota -> CastingContext -> ActionResult
setItemLore stack ctx =
    spell2Inputs stack ctx getItemStack getIotaList


-- ============================================================
-- Helper: convert Iota to display string
-- ============================================================

getIotaDisplayString : Iota -> String
getIotaDisplayString iota =
    case iota of
        Number n ->
            String.fromFloat n

        Vector ( x, y, z ) ->
            "(" ++ String.fromFloat x ++ ", " ++ String.fromFloat y ++ ", " ++ String.fromFloat z ++ ")"

        Boolean b ->
            if b then
                "true"

            else
                "false"

        Entity e ->
            e

        Null ->
            "null"

        Identifier id ->
            id

        Display t _ ->
            t

        ItemStack id _ ->
            id

        Garbage _ ->
            "garbage"

        IotaList _ ->
            "[list]"

        PatternIota p _ ->
            p.displayName

        OpenParenthesis _ ->
            "[introspection]"

        -- hexcellular iota
        Property key ->
            key

        -- moreiotas iotas
        MString s ->
            s

        MMatrix s ->
            s

        MIotaType s ->
            s

        MEntityType s ->
            s

        MItemType s ->
            s

        MItemStack id _ ->
            id
