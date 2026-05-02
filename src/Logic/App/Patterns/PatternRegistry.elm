module Logic.App.Patterns.PatternRegistry exposing (getPatternFromName, getPatternFromSignature, numberLiteralGenerator, patternRegistry, unknownPattern)

import Array exposing (Array)
import Array.Extra as Array
import Dict exposing (Dict)
import FontAwesome.Solid exposing (signature)
import Logic.App.Grid exposing (centerMidpoints, drawPattern, gridpointToMidpoints)
import Logic.App.Patterns.Circles exposing (..)
import Logic.App.Patterns.GreatSpells exposing (..)
import Logic.App.Patterns.Hexpose exposing (..)
import Logic.App.Patterns.Hexcellular exposing (..)
import Logic.App.Patterns.HexFlow exposing (..)
import Logic.App.Patterns.Lists exposing (..)
import Logic.App.Patterns.Math exposing (..)
import Logic.App.Patterns.Misc exposing (..)
import Logic.App.Patterns.MoreIotas exposing (..)
import Logic.App.Patterns.OperatorUtils exposing (getIotaList, makeConstant, mapNothingToMissingIota, moveNothingsToFront, nanOrInfinityCheck, spellNoInput)
import Logic.App.Patterns.ReadWrite exposing (..)
import Logic.App.Patterns.Selectors exposing (..)
import Logic.App.Patterns.Spells exposing (..)
import Logic.App.Patterns.Stack exposing (..)
import Logic.App.Types exposing (ActionResult, ApplyToStackResult(..), CastingContext, Direction(..), HeldItem(..), Iota(..), IotaType(..), MetaActionMsg(..), Mishap(..), Pattern)
import Logic.App.Utils.RegexPatterns exposing (bookkeepersPattern)
import Logic.App.Utils.Utils exposing (ifThenElse, unshift)
import Ports.HexNumGen as HexNumGen
import Regex
import Set exposing (Set)
import Settings.Theme exposing (..)


noAction : Array Iota -> CastingContext -> ActionResult
noAction stack ctx =
    { stack = stack, ctx = ctx, success = True }


unknownPattern : Pattern
unknownPattern =
    { signature = ""
    , startDirection = East
    , action = \stack ctx -> { stack = unshift (Garbage InvalidPattern) stack, ctx = ctx, success = False }
    , metaAction = None
    , displayName = "Unknown Pattern"
    , internalName = "unknown"
    , color = accent3
    , outputOptions = []
    , selectedOutput = Nothing
    , active = True
    }


getPatternFromSignature : Maybe (Dict String ( String, Direction, Iota )) -> String -> Pattern
getPatternFromSignature maybeMacros signature =
    case List.head <| List.filter (\regPattern -> regPattern.signature == signature) patternRegistry of
        Just a ->
            a

        Nothing ->
            if String.startsWith "aqaa" signature then
                numberLiteralGenerator signature False

            else if String.startsWith "dedd" signature then
                numberLiteralGenerator signature True

            else
                let
                    parseBookkeeperResult =
                        parseBookkeeperSignature signature
                in
                if parseBookkeeperResult.internalName /= "unknown" then
                    parseBookkeeperResult

                else
                    let
                        getGreatSpell =
                            let
                                getCenterdMidpoints sig direction =
                                    (drawPattern 0 0 { unknownPattern | signature = sig, startDirection = direction }).points
                                        |> List.concatMap gridpointToMidpoints
                                        |> centerMidpoints
                                        |> Set.fromList

                                greatSpellMatches =
                                    List.concatMap
                                        (\direction ->
                                            List.filter
                                                (\greatSpell ->
                                                    getCenterdMidpoints signature direction == getCenterdMidpoints greatSpell.signature East
                                                )
                                                greatSpellRegistry
                                        )
                                        [ Northeast, East, Southeast, Southwest, West, Northwest ]
                            in
                            Maybe.withDefault { unknownPattern | signature = signature, displayName = "Pattern " ++ "\"" ++ signature ++ "\"" } <| List.head greatSpellMatches
                    in
                    case maybeMacros of
                        Just macros ->
                            case Dict.get signature macros of
                                Just value ->
                                    case value of
                                        ( displayName, direction, _ ) ->
                                            { signature = signature
                                            , internalName = ""
                                            , action = noAction
                                            , metaAction = None
                                            , displayName = displayName
                                            , color = accent1
                                            , outputOptions = []
                                            , active = True
                                            , selectedOutput = Nothing
                                            , startDirection = direction
                                            }

                                Nothing ->
                                    getGreatSpell

                        Nothing ->
                            getGreatSpell


getPatternFromName : Maybe (Dict String ( String, Direction, Iota )) -> String -> ( Pattern, Cmd msg )
getPatternFromName maybeMacros name =
    case List.head <| List.filter (\regPattern -> regPattern.displayName == name || regPattern.internalName == name || regPattern.signature == name) patternRegistry of
        Just a ->
            ( a, Cmd.none )

        Nothing ->
            case String.toFloat name of
                Just number ->
                    ( unknownPattern, HexNumGen.sendNumber number )

                Nothing ->
                    let
                        regexMatch =
                            Regex.find bookkeepersPattern name
                                |> List.map (\x -> x.match)
                                |> List.head
                                |> Maybe.withDefault ""
                                |> String.trim
                    in
                    if regexMatch == String.trim name then
                        ( parseBookkeeperCode name, Cmd.none )

                    else
                        case maybeMacros of
                            Just macros ->
                                case Dict.get name macros of
                                    Just value ->
                                        case value of
                                            ( displayName, direction, _ ) ->
                                                ( { signature = name
                                                  , internalName = ""
                                                  , action = noAction
                                                  , metaAction = None
                                                  , displayName = displayName
                                                  , color = accent1
                                                  , outputOptions = []
                                                  , active = True
                                                  , selectedOutput = Nothing
                                                  , startDirection = direction
                                                  }
                                                , Cmd.none
                                                )

                                    Nothing ->
                                        case
                                            Dict.toList macros
                                                |> List.filter
                                                    (\x ->
                                                        case x of
                                                            ( _, ( displayName, _, _ ) ) ->
                                                                displayName == name
                                                    )
                                                |> List.head
                                        of
                                            Just ( signature, ( displayName, direction, _ ) ) ->
                                                ( { signature = signature
                                                  , internalName = ""
                                                  , action = noAction
                                                  , metaAction = None
                                                  , displayName = displayName
                                                  , color = accent1
                                                  , outputOptions = []
                                                  , active = True
                                                  , selectedOutput = Nothing
                                                  , startDirection = direction
                                                  }
                                                , Cmd.none
                                                )

                                            Nothing ->
                                                if Regex.contains Logic.App.Utils.RegexPatterns.angleSignaturePattern name then
                                                    ( getPatternFromSignature maybeMacros name, Cmd.none )

                                                else
                                                    ( unknownPattern, Cmd.none )

                            Nothing ->
                                if Regex.contains Logic.App.Utils.RegexPatterns.angleSignaturePattern name then
                                    ( getPatternFromSignature maybeMacros name, Cmd.none )

                                else
                                    ( unknownPattern, Cmd.none )


parseBookkeeperCode : String -> Pattern
parseBookkeeperCode code =
    -- I'm very good at naming things
    if code == "-" then
        { signature = ""
        , internalName = "mask"
        , action = mask [ "-" ]
        , metaAction = None
        , displayName = "Bookkeeper's Gambit: -"
        , color = accent1
        , outputOptions = []
        , selectedOutput = Nothing
        , active = True
        , startDirection = East
        }

    else
        let
            codeList =
                String.split "" code

            toAngleSignature codeSegment accumulator =
                case codeSegment of
                    "-" ->
                        if accumulator.prevSeg == "-" then
                            { prevSeg = codeSegment, signature = accumulator.signature ++ "w" }

                        else if accumulator.prevSeg == "v" then
                            { prevSeg = codeSegment, signature = accumulator.signature ++ "e" }

                        else
                            { accumulator | prevSeg = codeSegment }

                    "v" ->
                        if accumulator.prevSeg == "-" then
                            { prevSeg = codeSegment, signature = accumulator.signature ++ "ea" }

                        else if accumulator.prevSeg == "v" then
                            { prevSeg = codeSegment, signature = accumulator.signature ++ "da" }

                        else
                            { prevSeg = codeSegment, signature = accumulator.signature ++ "a" }

                    _ ->
                        accumulator

            signature =
                (List.foldl toAngleSignature { prevSeg = "", signature = "" } codeList).signature
        in
        { signature = signature
        , internalName = "mask"
        , action = mask (String.split "" code)
        , metaAction = None
        , displayName = "Bookkeeper's Gambit: " ++ code
        , color = accent1
        , outputOptions = []
        , selectedOutput = Nothing
        , active = True
        , startDirection = ifThenElse (String.startsWith "v" code) Southeast East
        }


parseBookkeeperSignature : String -> Pattern
parseBookkeeperSignature signature =
    if signature == "" then
        { signature = signature
        , internalName = "mask"
        , action = mask [ "-" ]
        , metaAction = None
        , displayName = "Bookkeeper's Gambit: -"
        , color = accent1
        , outputOptions = []
        , selectedOutput = Nothing
        , active = True
        , startDirection = East
        }

    else
        let
            angleList =
                String.split "" signature

            parseSignature angle accumulatorResult =
                case accumulatorResult of
                    Ok accumulator ->
                        if List.length accumulator == 0 then
                            if angle == "e" then
                                Ok <| [ "\\", "-" ] ++ accumulator

                            else if angle == "w" then
                                Ok <| [ "-", "-" ] ++ accumulator

                            else if angle == "a" then
                                Ok <| "v" :: accumulator

                            else
                                Err accumulator

                        else
                            case Maybe.withDefault "" (List.head accumulator) of
                                "\\" ->
                                    if angle == "a" then
                                        Ok <| "v" :: Maybe.withDefault [] (List.tail accumulator)

                                    else
                                        Err accumulator

                                "v" ->
                                    if angle == "e" then
                                        Ok <| "-" :: accumulator

                                    else if angle == "d" then
                                        Ok <| "\\" :: accumulator

                                    else
                                        Err accumulator

                                "-" ->
                                    if angle == "w" then
                                        Ok <| "-" :: accumulator

                                    else if angle == "e" then
                                        Ok <| [ "\\", "-" ] ++ Maybe.withDefault [] (List.tail accumulator)

                                    else
                                        Err accumulator

                                _ ->
                                    Err accumulator

                    Err _ ->
                        accumulatorResult

            maskCodeResult =
                case List.foldl parseSignature (Ok []) angleList of
                    Ok maskCode ->
                        if Maybe.withDefault "" (List.head maskCode) == "\\" then
                            Err <| List.reverse maskCode

                        else
                            Ok <| List.reverse maskCode

                    Err maskCode ->
                        Err <| List.reverse maskCode
        in
        case maskCodeResult of
            Ok maskCode ->
                { signature = signature
                , internalName = "mask"
                , action = mask maskCode
                , metaAction = None
                , displayName = "Bookkeeper's Gambit: " ++ String.concat maskCode
                , color = accent1
                , outputOptions = []
                , selectedOutput = Nothing
                , active = True
                , startDirection = East -- Todo: make this southeast if starting with v
                }

            Err _ ->
                unknownPattern


greatSpellRegistry : List Pattern
greatSpellRegistry =
    [ { signature = "qdwedadedae", internalName = "create_lava", action = createLava, displayName = "Create Lava", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qqqqaawawaedd", internalName = "potion/regeneration", action = potion, displayName = "White Sun's Zenith", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qqqaawawaeqdd", internalName = "potion/night_vision", action = potionFixedPotency, displayName = "Blue Sun's Zenith", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qqaawawaeqqdd", internalName = "potion/absorption", action = potion, displayName = "Black Sun's Zenith", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qaawawaeqqqdd", internalName = "potion/haste", action = potion, displayName = "Red Sun's Zenith", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "aawawaeqqqqdd", internalName = "potion/strength", action = potion, displayName = "Green Sun's Zenith", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "waadwawdaaweewq", internalName = "lightning", action = lightning, displayName = "Summon Lightning", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "wwweeewwweewdawdwad", internalName = "summon_rain", action = spellNoInput, displayName = "Summon Rain", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "eeewwweeewwaqqddqdqd", internalName = "dispel_rain", action = spellNoInput, displayName = "Dispel Rain", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "wwwqqqwwwqqeqqwwwqqwqqdqqqqqdqq", internalName = "teleport", action = teleport, displayName = "Greater Teleport", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "waeawaeqqqwqwqqwq", internalName = "sentinel/create/great", action = sentinelCreate, displayName = "Summon Greater Sentinel", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "aqqqaqwwaqqqqqeqaqqqawwqwqwqwqwqw", internalName = "craft/battery", action = craftPhial, displayName = "Craft Phial", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qeqwqwqwqwqeqaeqeaqeqaeqaqded", internalName = "brainsweep", action = brainsweep, displayName = "Flay Mind", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    ]
        |> List.map
            (\pattern ->
                { signature = pattern.signature
                , startDirection = pattern.startDirection
                , internalName = pattern.internalName
                , action = pattern.action
                , metaAction = None
                , displayName = pattern.displayName
                , outputOptions = pattern.outputOptions
                , selectedOutput = pattern.selectedOutput
                , color = accent1
                , active = True
                }
            )


patternRegistry : List Pattern
patternRegistry =
    [ { signature = "wawawddew", internalName = "interop/gravity/get", action = gravityGet, displayName = "Gravitational Purification", outputOptions = [ VectorType ], selectedOutput = Just ( VectorType, Vector ( 0, -1, 0 ) ), startDirection = East }
    , { signature = "wdwdwaaqw", internalName = "interop/gravity/set", action = gravitySet, displayName = "Alter Gravity", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "aawawwawwa", internalName = "interop/pehkui/get", action = pekhuiGet, displayName = "Gulliver's Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 1 ), startDirection = East }
    , { signature = "ddwdwwdwwd", internalName = "interop/pehkui/set", action = pekhuiSet, displayName = "Alter Scale", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qaq", internalName = "get_caster", action = getCaster, displayName = "Mind's Reflection", outputOptions = [], selectedOutput = Nothing, startDirection = Northeast }
    , { signature = "aa", internalName = "entity_pos/eye", action = entityPos, displayName = "Compass' Purification", outputOptions = [ VectorType ], selectedOutput = Just ( VectorType, Vector ( 0, 0, 0 ) ), startDirection = East }
    , { signature = "dd", internalName = "entity_pos/foot", action = entityPos, displayName = "Compass' Purification II", outputOptions = [ VectorType ], selectedOutput = Just ( VectorType, Vector ( 0, 0, 0 ) ), startDirection = East }
    , { signature = "wa", internalName = "get_entity_look", action = getEntityLook, displayName = "Alidade's Purification", outputOptions = [ VectorType ], selectedOutput = Just ( VectorType, Vector ( 0, 0, 0 ) ), startDirection = East }
    , { signature = "awq", internalName = "get_entity_height", action = getEntityHeight, displayName = "Stadiometer's Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = East }
    , { signature = "wq", internalName = "get_entity_velocity", action = getEntityVelocity, displayName = "Pace Purification", outputOptions = [ VectorType ], selectedOutput = Just ( VectorType, Vector ( 0, 0, 0 ) ), startDirection = East }
    , { signature = "wqaawdd", internalName = "raycast", action = raycast, displayName = "Archer's Distillation", outputOptions = [ VectorType, NullType ], selectedOutput = Just ( VectorType, Vector ( 0, 0, 0 ) ), startDirection = East }
    , { signature = "weddwaa", internalName = "raycast/axis", action = raycastAxis, displayName = "Architect's Distillation", outputOptions = [ VectorType, NullType ], selectedOutput = Just ( VectorType, Vector ( 0, 0, 0 ) ), startDirection = East }
    , { signature = "weaqa", internalName = "raycast/entity", action = raycastEntity, displayName = "Scout's Distillation", outputOptions = [ EntityType, NullType ], selectedOutput = Just ( NullType, Null ), startDirection = East }
    , { signature = "eaqwqae", internalName = "circle/impetus_pos", action = spellNoInput, displayName = "Waystone Reflection", outputOptions = [ VectorType ], selectedOutput = Just ( VectorType, Vector ( 0, 0, 0 ) ), startDirection = East }
    , { signature = "eaqwqaewede", internalName = "circle/impetus_dir", action = spellNoInput, displayName = "Lodestone Reflection", outputOptions = [ VectorType ], selectedOutput = Just ( VectorType, Vector ( 0, 0, 0 ) ), startDirection = East }
    , { signature = "eaqwqaewdd", internalName = "circle/bounds/min", action = spellNoInput, displayName = "Lesser Fold Reflection", outputOptions = [ VectorType ], selectedOutput = Just ( VectorType, Vector ( 0, 0, 0 ) ), startDirection = East }
    , { signature = "aqwqawaaqa", internalName = "circle/bounds/max", action = spellNoInput, displayName = "Greater Fold Reflection", outputOptions = [ VectorType ], selectedOutput = Just ( VectorType, Vector ( 0, 0, 0 ) ), startDirection = East }
    , { signature = "aawdd", internalName = "swap", action = swap, displayName = "Jester's Gambit", outputOptions = [], selectedOutput = Nothing, startDirection = Northeast }
    , { signature = "aaeaa", internalName = "rotate", action = rotate, displayName = "Rotation Gambit", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "ddqdd", internalName = "rotate_reverse", action = rotateReverse, displayName = "Rotation Gambit II", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "aadaa", internalName = "duplicate", action = duplicate, displayName = "Gemini Decomposition", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "aaedd", internalName = "over", action = over, displayName = "Prospector's Gambit", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "ddqaa", internalName = "tuck", action = tuck, displayName = "Undertaker's Gambit", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "aadadaaw", internalName = "two_dup", action = dup2, displayName = "Dioscuri Gambit", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qwaeawqaeaqa", internalName = "stack_len", action = stackLength, displayName = "Flock's Reflection", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "aadaadaa", internalName = "duplicate_n", action = duplicateN, displayName = "Gemini Gambit", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "ddad", internalName = "fisherman", action = fisherman, displayName = "Fisherman's Gambit", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "aada", internalName = "fisherman/copy", action = fishermanCopy, displayName = "Fisherman's Gambit II", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qaawdde", internalName = "swizzle", action = swizzle, displayName = "Swindler's Gambit", outputOptions = [], selectedOutput = Nothing, startDirection = East } -- do this
    , { signature = "waaw", internalName = "add", action = add, displayName = "Additive Distillation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "wddw", internalName = "sub", action = subtract, displayName = "Subtractive Distillation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "waqaw", internalName = "mul_dot", action = mulDot, displayName = "Multiplicative Distillation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "wdedw", internalName = "div_cross", action = divCross, displayName = "Division Distillation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "wqaqw", internalName = "abs_len", action = absLen, displayName = "Length Purification", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "wedew", internalName = "pow_proj", action = powProj, displayName = "Power Distillation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "ewq", internalName = "floor", action = floorAction, displayName = "Floor Purification", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qwe", internalName = "ceil", action = ceilAction, displayName = "Ceiling Purification", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "eqqqqq", internalName = "construct_vec", action = constructVector, displayName = "Vector Exaltation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qeeeee", internalName = "deconstruct_vec", action = deconstructVector, displayName = "Vector Disintegration", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qqqqqaww", internalName = "coerce_axial", action = coerceAxial, displayName = "Axial Purification", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "wdw", internalName = "and", action = andBool, displayName = "Conjunction Distillation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "waw", internalName = "or", action = orBool, displayName = "Disjunction Distillation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "dwa", internalName = "xor", action = xorBool, displayName = "Exclusion Distillation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "e", internalName = "greater", action = greaterThan, displayName = "Maximus Distillation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "q", internalName = "less", action = lessThan, displayName = "Minimus Distillation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "ee", internalName = "greater_eq", action = greaterThanOrEqualTo, displayName = "Maximus Distillation II", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qq", internalName = "less_eq", action = lessThanOrEqualTo, displayName = "Minimus Distillation II", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "ad", internalName = "equals", action = equalTo, displayName = "Equality Distillation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "da", internalName = "not_equals", action = notEqualTo, displayName = "Inequality Distillation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "dw", internalName = "not", action = invertBool, displayName = "Negation Purification", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "aw", internalName = "bool_coerce", action = boolCoerce, displayName = "Augur's Purification", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "awdd", internalName = "if", action = ifBool, displayName = "Augur's Exaltation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "eqqq", internalName = "random", action = spellNoInput, displayName = "Entropy Reflection", outputOptions = [NumberType], selectedOutput = Just (NumberType, Number 0), startDirection = East }
    , { signature = "qqqqqaa", internalName = "sin", action = sine, displayName = "Sine Purification", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qqqqqad", internalName = "cos", action = cosine, displayName = "Cosine Purification", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "wqqqqqadq", internalName = "tan", action = tangent, displayName = "Tangent Purification", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "ddeeeee", internalName = "arcsin", action = arcsin, displayName = "Inverse Sine Purification", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "adeeeee", internalName = "arccos", action = arccos, displayName = "Inverse Cosine Purification", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "eadeeeeew", internalName = "arctan", action = arctan, displayName = "Inverse Tangent Purification", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "eqaqe", internalName = "logarithm", action = logarithm, displayName = "Logarithmic Distillation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "addwaad", internalName = "modulo", action = modulo, displayName = "Modulus Distillation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "wdweaqa", internalName = "and_bit", action = andBit, displayName = "Intersection Distillation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "waweaqa", internalName = "or_bit", action = orBit, displayName = "Unifying Distillation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "dwaeaqa", internalName = "xor_bit", action = xorBit, displayName = "Exclusionary Distillation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "dweaqa", internalName = "not_bit", action = notBit, displayName = "Inversion Purification", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "aweaqa", internalName = "to_set", action = toSet, displayName = "Uniqueness Purification", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "de", internalName = "print", action = print, displayName = "Reveal", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "aawaawaa", internalName = "explode", action = explode, displayName = "Explosion", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "ddwddwdd", internalName = "explode/fire", action = explodeFire, displayName = "Fireball", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "awqqqwaqw", internalName = "add_motion", action = addMotion, displayName = "Impulse", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "awqqqwaq", internalName = "blink", action = blink, displayName = "Blink", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qaqqqqq", internalName = "break_block", action = breakBlock, displayName = "Break Block", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "eeeeede", internalName = "place_block", action = placeBlock, displayName = "Place Block", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "awddwqawqwawq", internalName = "colorize", action = colorize, displayName = "Internalize Pigment", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "aqawqadaq", internalName = "create_water", action = createWater, displayName = "Create Water", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "dedwedade", internalName = "destroy_water", action = destroyWater, displayName = "Destroy Liquid", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "aaqawawa", internalName = "ignite", action = ignite, displayName = "Ignite Block", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "ddedwdwd", internalName = "extinguish", action = extinguish, displayName = "Extinguish Area", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qqa", internalName = "conjure_block", action = conjureBlock, displayName = "Conjure Block", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qqd", internalName = "conjure_light", action = conjureLight, displayName = "Conjure Light", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "wqaqwawqaqw", internalName = "bonemeal", action = bonemeal, displayName = "Overgrow", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qqqqqwaeaeaeaeaea", internalName = "recharge", action = recharge, displayName = "Recharge Item", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qdqawwaww", internalName = "erase", action = erase, displayName = "Erase Item", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "wqaqwd", internalName = "edify", action = edify, displayName = "Edify Sapling", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "adaa", internalName = "beep", action = beep, displayName = "Make Note", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "waqqqqq", internalName = "craft/cypher", action = craftArtifact Cypher, displayName = "Craft Cypher", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "wwaqqqqqeaqeaeqqqeaeq", internalName = "craft/trinket", action = craftArtifact Trinket, displayName = "Craft Trinket", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "wwaqqqqqeawqwqwqwqwqwwqqeadaeqqeqqeadaeqq", internalName = "craft/artifact", action = craftArtifact Artifact, displayName = "Craft Artifact", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qqqqqaqwawaw", internalName = "potion/weakness", action = potion, displayName = "White Sun's Nadir", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qqqqqawwawawd", internalName = "potion/levitation", action = potionFixedPotency, displayName = "Blue Sun's Nadir", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qqqqqaewawawe", internalName = "potion/wither", action = potion, displayName = "Black Sun's Nadir", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qqqqqadwawaww", internalName = "potion/poison", action = potion, displayName = "Red Sun's Nadir", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qqqqqadwawaw", internalName = "potion/slowness", action = potion, displayName = "Green Sun's Nadir", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "waeawae", internalName = "sentinel/create", action = sentinelCreate, displayName = "Summon Sentinel", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qdwdqdw", internalName = "sentinel/destroy", action = sentinelDestroy, displayName = "Banish Sentinel", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "waeawaede", internalName = "sentinel/get_pos", action = sentinelGetPos, displayName = "Locate Sentinel", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "waeawaedwa", internalName = "sentinel/wayfind", action = sentinelWayfind, displayName = "Wayfind Sentinel", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qqqwqqqqqaq", internalName = "akashic/read", action = akashicRead, displayName = "Akasha's Distillation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "eeeweeeeede", internalName = "akashic/write", action = akashicWrite, displayName = "Akasha's Gambit", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "aqdee", internalName = "halt", action = noAction, displayName = "Charon's Gambit", outputOptions = [], selectedOutput = Nothing, startDirection = Southwest }
    , { signature = "aqqqqq", internalName = "read", action = read, displayName = "Scribe's Reflection", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "wawqwqwqwqwqw", internalName = "read/entity", action = readChronical, displayName = "Chronicler's Purification", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "deeeee", internalName = "write", action = write, displayName = "Scribe's Gambit", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "wdwewewewewew", internalName = "write/entity", action = writeChronical, displayName = "Chronicler's Gambit", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "aqqqqqe", internalName = "readable", action = readable, displayName = "Auditor's Reflection", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "wawqwqwqwqwqwew", internalName = "readable/entity", action = makeConstant (Boolean False), displayName = "Auditor's Purification", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "deeeeeq", internalName = "writable", action = writable, displayName = "Assessor's Reflection", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "wdwewewewewewqw", internalName = "writable/entity", action = makeConstant (Boolean False), displayName = "Assessor's Purification", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qeewdweddw", internalName = "read/local", action = readLocal, displayName = "Muninn's Reflection", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "eqqwawqaaw", internalName = "write/local", action = writeLocal, displayName = "Huginn's Gambit", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "d", internalName = "const/null", action = makeConstant Null, displayName = "Nullary Reflection", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "aqae", internalName = "const/true", action = makeConstant (Boolean True), displayName = "True Reflection", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "dedq", internalName = "const/false", action = makeConstant (Boolean False), displayName = "False Reflection", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qqqqqea", internalName = "const/vec/px", action = makeConstant (Vector ( 1, 0, 0 )), displayName = "Vector Reflection +X", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qqqqqew", internalName = "const/vec/py", action = makeConstant (Vector ( 0, 1, 0 )), displayName = "Vector Reflection +Y", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qqqqqed", internalName = "const/vec/pz", action = makeConstant (Vector ( 0, 0, 1 )), displayName = "Vector Reflection +Z", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "eeeeeqa", internalName = "const/vec/nx", action = makeConstant (Vector ( -1, 0, 0 )), displayName = "Vector Reflection -X", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "eeeeeqw", internalName = "const/vec/ny", action = makeConstant (Vector ( 0, -1, 0 )), displayName = "Vector Reflection -Y", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "eeeeeqd", internalName = "const/vec/nz", action = makeConstant (Vector ( 0, 0, -1 )), displayName = "Vector Reflection -Z", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qqqqq", internalName = "const/vec/0", action = makeConstant (Vector ( 0, 0, 0 )), displayName = "Vector Reflection Zero", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qdwdq", internalName = "const/double/pi", action = makeConstant (Number pi), displayName = "Arc's Reflection", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "eawae", internalName = "const/double/tau", action = makeConstant (Number (pi * 2)), displayName = "Circle's Reflection", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "aaq", internalName = "const/double/e", action = makeConstant (Number e), displayName = "Euler's Reflection", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qqqqqdaqa", internalName = "get_entity", action = getEntity, displayName = "Entity Purification", outputOptions = [ EntityType, NullType ], selectedOutput = Just ( NullType, Null ), startDirection = East }
    , { signature = "qqqqqdaqaawa", internalName = "get_entity/animal", action = getEntity, displayName = "Entity Purification: Animal", outputOptions = [ EntityType, NullType ], selectedOutput = Just ( NullType, Null ), startDirection = East }
    , { signature = "qqqqqdaqaawq", internalName = "get_entity/monster", action = getEntity, displayName = "Entity Purification: Monster", outputOptions = [ EntityType, NullType ], selectedOutput = Just ( NullType, Null ), startDirection = East }
    , { signature = "qqqqqdaqaaww", internalName = "get_entity/item", action = getEntity, displayName = "Entity Purification: Item", outputOptions = [ EntityType, NullType ], selectedOutput = Just ( NullType, Null ), startDirection = East }
    , { signature = "qqqqqdaqaawe", internalName = "get_entity/player", action = getEntity, displayName = "Entity Purification: Player", outputOptions = [ EntityType, NullType ], selectedOutput = Just ( NullType, Null ), startDirection = East }
    , { signature = "qqqqqdaqaawd", internalName = "get_entity/living", action = getEntity, displayName = "Entity Purification: Living", outputOptions = [ EntityType, NullType ], selectedOutput = Just ( NullType, Null ), startDirection = East }
    , { signature = "qqqqqwded", internalName = "zone_entity", action = zoneEntity, displayName = "Zone Distillation: Any", outputOptions = [ IotaListType EntityType ], selectedOutput = Just ( IotaListType EntityType, IotaList Array.empty ), startDirection = East }
    , { signature = "qqqqqwdeddwa", internalName = "zone_entity/animal", action = zoneEntity, displayName = "Zone Distillation: Animal", outputOptions = [ IotaListType EntityType ], selectedOutput = Just ( IotaListType EntityType, IotaList Array.empty ), startDirection = East }
    , { signature = "eeeeewaqaawa", internalName = "zone_entity/not_animal", action = zoneEntity, displayName = "Zone Distillation: Non-Animal", outputOptions = [ IotaListType EntityType ], selectedOutput = Just ( IotaListType EntityType, IotaList Array.empty ), startDirection = East }
    , { signature = "qqqqqwdeddwq", internalName = "zone_entity/monster", action = zoneEntity, displayName = "Zone Distillation: Monster", outputOptions = [ IotaListType EntityType ], selectedOutput = Just ( IotaListType EntityType, IotaList Array.empty ), startDirection = East }
    , { signature = "eeeeewaqaawq", internalName = "zone_entity/not_monster", action = zoneEntity, displayName = "Zone Distillation: Non-Monster", outputOptions = [ IotaListType EntityType ], selectedOutput = Just ( IotaListType EntityType, IotaList Array.empty ), startDirection = East }
    , { signature = "qqqqqwdeddww", internalName = "zone_entity/item", action = zoneEntity, displayName = "Zone Distillation: Item", outputOptions = [ IotaListType EntityType ], selectedOutput = Just ( IotaListType EntityType, IotaList Array.empty ), startDirection = East }
    , { signature = "eeeeewaqaaww", internalName = "zone_entity/not_item", action = zoneEntity, displayName = "Zone Distillation: Non-Item", outputOptions = [ IotaListType EntityType ], selectedOutput = Just ( IotaListType EntityType, IotaList Array.empty ), startDirection = East }
    , { signature = "qqqqqwdeddwe", internalName = "zone_entity/player", action = zoneEntity, displayName = "Zone Distillation: Player", outputOptions = [ IotaListType EntityType ], selectedOutput = Just ( IotaListType EntityType, IotaList Array.empty ), startDirection = East }
    , { signature = "eeeeewaqaawe", internalName = "zone_entity/not_player", action = zoneEntity, displayName = "Zone Distillation: Non-Player", outputOptions = [ IotaListType EntityType ], selectedOutput = Just ( IotaListType EntityType, IotaList Array.empty ), startDirection = East }
    , { signature = "qqqqqwdeddwd", internalName = "zone_entity/living", action = zoneEntity, displayName = "Zone Distillation: Living", outputOptions = [ IotaListType EntityType ], selectedOutput = Just ( IotaListType EntityType, IotaList Array.empty ), startDirection = East }
    , { signature = "eeeeewaqaawd", internalName = "zone_entity/not_living", action = zoneEntity, displayName = "Zone Distillation: Non-Living", outputOptions = [ IotaListType EntityType ], selectedOutput = Just ( IotaListType EntityType, IotaList Array.empty ), startDirection = East }
    , { signature = "edqde", internalName = "append", action = append, displayName = "Integration Distillation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qaeaq", internalName = "concat", action = concat, displayName = "Combination Distillation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "deeed", internalName = "index", action = index, displayName = "Selection Distillation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "aqaeaq", internalName = "list_size", action = listSize, displayName = "Abacus Purification", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "adeeed", internalName = "singleton", action = singleton, displayName = "Single's Purification", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qqaeaae", internalName = "empty_list", action = makeConstant (IotaList Array.empty), displayName = "Vacant Reflection", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qqqaede", internalName = "reverse_list", action = reverseList, displayName = "Retrograde Purification", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "ewdqdwe", internalName = "last_n_list", action = lastNList, displayName = "Flock's Gambit", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qwaeawq", internalName = "splat", action = splat, displayName = "Flock's Disintegration", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "dedqde", internalName = "index_of", action = indexOf, displayName = "Locator's Distillation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "edqdewaqa", internalName = "list_remove", action = listRemove, displayName = "Excisor's Distillation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qaeaqwded", internalName = "slice", action = slice, displayName = "Selection Exaltation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "wqaeaqw", internalName = "modify_in_place", action = modifyinPlace, displayName = "Surgeon's Exaltation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "ddewedd", internalName = "construct", action = construct, displayName = "Speaker's Distillation", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "aaqwqaa", internalName = "deconstruct", action = deconstruct, displayName = "Speaker's Decomposition", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "qqqaw", internalName = "escape", action = noAction, displayName = "Consideration", outputOptions = [], selectedOutput = Nothing, startDirection = West }
    , { signature = "qqq", internalName = "open_paren", action = makeConstant (OpenParenthesis Array.empty), displayName = "Introspection", outputOptions = [], selectedOutput = Nothing, startDirection = West }
    , { signature = "eee", internalName = "close_paren", action = noAction, displayName = "Retrospection", outputOptions = [], selectedOutput = Nothing, startDirection = East }
    , { signature = "deaqq", internalName = "eval", action = noAction, displayName = "Hermes' Gambit", outputOptions = [], selectedOutput = Nothing, startDirection = Southeast }
    , { signature = "dadad", internalName = "for_each", action = noAction, displayName = "Thoth's Gambit", outputOptions = [], selectedOutput = Nothing, startDirection = Northeast }
    , { signature = "awaawa", internalName = "save_macro", action = saveMacro, displayName = "Save Macro", outputOptions = [], selectedOutput = Nothing, startDirection = Southeast }
    -- ==================== hexcellular patterns ====================
    , { signature = "aawe", internalName = "create_property", action = createProperty, displayName = "Schrodinger's Reflection", outputOptions = [ PropertyType ], selectedOutput = Just ( PropertyType, Property "new_property" ), startDirection = Southwest }
    , { signature = "aawd", internalName = "observe_property", action = observeProperty, displayName = "Observation Purification", outputOptions = [ NullType, NumberType, VectorType ], selectedOutput = Just ( NullType, Null ), startDirection = Southwest }
    , { signature = "aawq", internalName = "set_property", action = setProperty, displayName = "Schrodinger's Gambit", outputOptions = [], selectedOutput = Nothing, startDirection = Southwest }
    , { signature = "aawa", internalName = "readonly_property", action = readonlyProperty, displayName = "Schrodinger's Purification", outputOptions = [ PropertyType ], selectedOutput = Just ( PropertyType, Property "new_property_readonly" ), startDirection = Southwest }
    -- ==================== end hexcellular ====================
    -- ==================== hexflow patterns ====================
    , { signature = "dadadad", internalName = "pure_map", action = pureMap, displayName = "Pure Map", outputOptions = [], selectedOutput = Nothing, startDirection = Northeast }
    , { signature = "waawadadad", internalName = "pure_reduce", action = pureReduce, displayName = "Pure Reduce", outputOptions = [], selectedOutput = Nothing, startDirection = Northeast }
    , { signature = "dadadqqaqqqqq", internalName = "for_range/cube", action = forRangeCube, displayName = "For Range: Cube", outputOptions = [ IotaListType VectorType ], selectedOutput = Just ( IotaListType VectorType, IotaList Array.empty ), startDirection = Northeast }
    , { signature = "dadadadqqaqqqqq", internalName = "for_range/cube/pure", action = forRangeCubePure, displayName = "For Range: Cube (Pure)", outputOptions = [ IotaListType VectorType ], selectedOutput = Just ( IotaListType VectorType, IotaList Array.empty ), startDirection = Northeast }
    , { signature = "dadadawwa", internalName = "for_range/line", action = forRangeLine, displayName = "For Range: Line", outputOptions = [ IotaListType VectorType ], selectedOutput = Just ( IotaListType VectorType, IotaList Array.empty ), startDirection = Northeast }
    , { signature = "dadadadawwa", internalName = "for_range/line/pure", action = forRangeLinePure, displayName = "For Range: Line (Pure)", outputOptions = [ IotaListType VectorType ], selectedOutput = Just ( IotaListType VectorType, IotaList Array.empty ), startDirection = Northeast }
    , { signature = "dadadqadadwdadadwdadaddwwawwaadaddwaaddad", internalName = "for_range/floodfill", action = forRangeFloodfill, displayName = "For Range: Floodfill", outputOptions = [ IotaListType VectorType ], selectedOutput = Just ( IotaListType VectorType, IotaList Array.empty ), startDirection = Northeast }
    , { signature = "dadadadqadadwdadadwdadaddwwawwaadaddwaaddad", internalName = "for_range/floodfill/pure", action = forRangeFloodfillPure, displayName = "For Range: Floodfill (Pure)", outputOptions = [ IotaListType VectorType ], selectedOutput = Just ( IotaListType VectorType, IotaList Array.empty ), startDirection = Northeast }
    , { signature = "edqdeqdwewwdwqwdwwew", internalName = "build_nested", action = buildNested, displayName = "Build Nested", outputOptions = [ IotaListType NullType ], selectedOutput = Just ( IotaListType NullType, IotaList Array.empty ), startDirection = Southwest }
    , { signature = "wdwawedqdewawdw", internalName = "nested_modify", action = nestedModify, displayName = "Nested Modify", outputOptions = [ IotaListType NullType ], selectedOutput = Just ( IotaListType NullType, IotaList Array.empty ), startDirection = Southwest }
    , { signature = "edqdewawddw", internalName = "mass_rotate", action = massRotate, displayName = "Mass Rotate", outputOptions = [ IotaListType NullType ], selectedOutput = Just ( IotaListType NullType, IotaList Array.empty ), startDirection = Southwest }
    , { signature = "qqqaww", internalName = "weak_escape", action = noAction, displayName = "Weak Escape", outputOptions = [], selectedOutput = Nothing, startDirection = West }
    , { signature = "dwdeaqqa", internalName = "call_stack", action = callStack, displayName = "Call Stack", outputOptions = [ NullType, NumberType, VectorType ], selectedOutput = Just ( NullType, Null ), startDirection = Southeast }
    -- ==================== end hexflow ====================
    -- ==================== moreiotas patterns ====================
    -- Strings
    , { signature = "awdwa", internalName = "string/empty", action = stringEmpty, displayName = "String Empty", outputOptions = [ MStringType ], selectedOutput = Just ( MStringType, MString "" ), startDirection = Southeast }
    , { signature = "awdwaaww", internalName = "string/space", action = stringSpace, displayName = "String Space", outputOptions = [ MStringType ], selectedOutput = Just ( MStringType, MString " " ), startDirection = Southeast }
    , { signature = "qa", internalName = "string/comma", action = stringComma, displayName = "String Comma", outputOptions = [ MStringType ], selectedOutput = Just ( MStringType, MString "," ), startDirection = East }
    , { signature = "waawaw", internalName = "string/newline", action = stringNewline, displayName = "String Newline", outputOptions = [ MStringType ], selectedOutput = Just ( MStringType, MString "\n" ), startDirection = East }
    , { signature = "awqwawqe", internalName = "string/block/get", action = stringBlockGet, displayName = "Get Block String", outputOptions = [ MStringType ], selectedOutput = Just ( MStringType, MString "" ), startDirection = East }
    , { signature = "dwewdweq", internalName = "string/block/set", action = stringBlockSet, displayName = "Set Block String", outputOptions = [], selectedOutput = Nothing, startDirection = West }
    , { signature = "waqa", internalName = "string/chat/caster", action = stringChatCaster, displayName = "Chat String (Caster)", outputOptions = [ MStringType ], selectedOutput = Just ( MStringType, MString "" ), startDirection = East }
    , { signature = "wded", internalName = "string/chat/all", action = stringChatAll, displayName = "Chat String (All)", outputOptions = [ MStringType ], selectedOutput = Just ( MStringType, MString "" ), startDirection = East }
    , { signature = "ewded", internalName = "string/chat/prefix/get", action = stringChatPrefixGet, displayName = "Get Chat Prefix", outputOptions = [ MStringType ], selectedOutput = Just ( MStringType, MString "" ), startDirection = Northeast }
    , { signature = "qwaqa", internalName = "string/chat/prefix/set", action = stringChatPrefixSet, displayName = "Set Chat Prefix", outputOptions = [], selectedOutput = Nothing, startDirection = Southeast }
    , { signature = "wawqwawaw", internalName = "string/iota", action = stringIota, displayName = "Iota To String", outputOptions = [ MStringType ], selectedOutput = Just ( MStringType, MString "" ), startDirection = East }
    , { signature = "wdwewdwdw", internalName = "string/action", action = stringAction, displayName = "Action To String", outputOptions = [ MStringType ], selectedOutput = Just ( MStringType, MString "" ), startDirection = Northwest }
    , { signature = "deqqeddqwqqqwq", internalName = "string/name/get", action = stringNameGet, displayName = "Get Name", outputOptions = [ MStringType ], selectedOutput = Just ( MStringType, MString "" ), startDirection = Southeast }
    , { signature = "aqeeqaaeweeewe", internalName = "string/name/set", action = stringNameSet, displayName = "Set Name", outputOptions = [], selectedOutput = Nothing, startDirection = Southwest }
    , { signature = "aqwaqa", internalName = "string/split", action = stringSplit, displayName = "Split String", outputOptions = [ IotaListType MStringType ], selectedOutput = Just ( IotaListType MStringType, IotaList Array.empty ), startDirection = East }
    , { signature = "aqwaq", internalName = "string/parse", action = stringParse, displayName = "Parse String", outputOptions = [ NumberType, NullType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = East }
    , { signature = "dwwdwwdwdd", internalName = "string/case", action = stringCase, displayName = "Case String", outputOptions = [ MStringType ], selectedOutput = Just ( MStringType, MString "" ), startDirection = West }
    -- Matrices
    , { signature = "awwaeawwaadwa", internalName = "matrix/make", action = matrixMake, displayName = "Make Matrix", outputOptions = [ MMatrixType ], selectedOutput = Just ( MMatrixType, MMatrix "" ), startDirection = Southwest }
    , { signature = "dwwdqdwwddawd", internalName = "matrix/unmake", action = matrixUnmake, displayName = "Unmake Matrix", outputOptions = [ IotaListType NumberType ], selectedOutput = Just ( IotaListType NumberType, IotaList Array.empty ), startDirection = Southeast }
    , { signature = "awwaeawwaqw", internalName = "matrix/identity", action = matrixIdentity, displayName = "Identity Matrix", outputOptions = [ MMatrixType ], selectedOutput = Just ( MMatrixType, MMatrix "" ), startDirection = Southwest }
    , { signature = "awwaeawwa", internalName = "matrix/zero", action = matrixZero, displayName = "Zero Matrix", outputOptions = [ MMatrixType ], selectedOutput = Just ( MMatrixType, MMatrix "" ), startDirection = Southwest }
    , { signature = "awwaeawwawawddw", internalName = "matrix/rotation", action = matrixRotation, displayName = "Rotation Matrix", outputOptions = [ MMatrixType ], selectedOutput = Just ( MMatrixType, MMatrix "" ), startDirection = Southwest }
    , { signature = "wwdqdwwdqaq", internalName = "matrix/inverse", action = matrixInverse, displayName = "Inverse Matrix", outputOptions = [ MMatrixType ], selectedOutput = Just ( MMatrixType, MMatrix "" ), startDirection = West }
    , { signature = "aeawwaeawaw", internalName = "matrix/determinant", action = matrixDeterminant, displayName = "Matrix Determinant", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = West }
    -- Types
    , { signature = "qawde", internalName = "type/entity", action = typeEntity, displayName = "Type Entity", outputOptions = [ MEntityTypeType ], selectedOutput = Just ( MEntityTypeType, MEntityType "minecraft:pig" ), startDirection = Southwest }
    , { signature = "awd", internalName = "type/iota", action = typeIota, displayName = "Type Iota", outputOptions = [ MIotaTypeType ], selectedOutput = Just ( MIotaTypeType, MIotaType "hexcasting:number" ), startDirection = Southwest }
    , { signature = "edeedqd", internalName = "type/item_held", action = typeItemHeld, displayName = "Type Item Held", outputOptions = [ MItemTypeType ], selectedOutput = Just ( MItemTypeType, MItemType "minecraft:stick" ), startDirection = Southwest }
    , { signature = "dadqqqqqdad", internalName = "get_entity/type", action = getEntityType, displayName = "Get Entity Type", outputOptions = [ MEntityTypeType ], selectedOutput = Just ( MEntityTypeType, MEntityType "minecraft:pig" ), startDirection = Northeast }
    , { signature = "waweeeeewaw", internalName = "zone_entity/type", action = zoneEntityType, displayName = "Zone Entity Type", outputOptions = [ IotaListType EntityType ], selectedOutput = Just ( IotaListType EntityType, IotaList Array.empty ), startDirection = Southeast }
    , { signature = "wdwqqqqqwdw", internalName = "zone_entity/not_type", action = zoneEntityNotType, displayName = "Zone Entity Not Type", outputOptions = [ IotaListType EntityType ], selectedOutput = Just ( IotaListType EntityType, IotaList Array.empty ), startDirection = Northeast }
    -- Item stacks (MoreIotas)
    , { signature = "adeq", internalName = "item/main_hand", action = itemGetMainHand, displayName = "Item Main Hand", outputOptions = [ MItemStackType ], selectedOutput = Just ( MItemStackType, MItemStack "minecraft:stick" 1 ), startDirection = East }
    , { signature = "qeda", internalName = "item/off_hand", action = itemGetOffHand, displayName = "Item Off Hand", outputOptions = [ MItemStackType ], selectedOutput = Just ( MItemStackType, MItemStack "minecraft:stick" 1 ), startDirection = East }
    , { signature = "aqwed", internalName = "item/inventory/stacks", action = itemGetInventoryStacks, displayName = "Get Inventory Stacks", outputOptions = [ IotaListType MItemStackType ], selectedOutput = Just ( IotaListType MItemStackType, IotaList Array.empty ), startDirection = Northeast }
    , { signature = "dewqa", internalName = "item/inventory/items", action = itemGetInventoryItems, displayName = "Get Inventory Items", outputOptions = [ IotaListType MItemTypeType ], selectedOutput = Just ( IotaListType MItemTypeType, IotaList Array.empty ), startDirection = Northeast }
    -- ==================== end moreiotas ====================
    -- ==================== hexpose patterns ====================
    -- Meta / Enlightenment
    , { signature = "awqaqqq", internalName = "am_enlightened", action = amEnlightened, displayName = "Epiphany Purification", outputOptions = [ BooleanType ], selectedOutput = Just ( BooleanType, Boolean False ), startDirection = Southeast }
    , { signature = "qqqaqqq", internalName = "is_brainswept", action = isBrainswept, displayName = "Sentience Purification", outputOptions = [ BooleanType ], selectedOutput = Just ( BooleanType, Boolean False ), startDirection = Southeast }
    -- Display iota creation
    , { signature = "awaqeeeee", internalName = "create_display", action = createDisplay, displayName = "Reading Purification", outputOptions = [ DisplayType ], selectedOutput = Just ( DisplayType, Display "" "" ), startDirection = Southwest }
    , { signature = "dwdeqqqqq", internalName = "display_children", action = displayChildren, displayName = "Parsing Purification", outputOptions = [ IotaListType DisplayType ], selectedOutput = Just ( IotaListType DisplayType, IotaList Array.empty ), startDirection = Southeast }
    , { signature = "awaqeeeeewded", internalName = "display_color", action = displayColor, displayName = "Lumiere Gambit", outputOptions = [ DisplayType ], selectedOutput = Just ( DisplayType, Display "" "" ), startDirection = Southwest }
    , { signature = "awaqeeeeedd", internalName = "display_bold", action = displayBold, displayName = "Gothic Gambit", outputOptions = [ DisplayType ], selectedOutput = Just ( DisplayType, Display "" "" ), startDirection = Southwest }
    , { signature = "awaqeeeeede", internalName = "display_italics", action = displayItalics, displayName = "Manutius' Gambit", outputOptions = [ DisplayType ], selectedOutput = Just ( DisplayType, Display "" "" ), startDirection = Southwest }
    , { signature = "awaqeeeeedw", internalName = "display_underline", action = displayUnderline, displayName = "Notetaker's Gambit", outputOptions = [ DisplayType ], selectedOutput = Just ( DisplayType, Display "" "" ), startDirection = Southwest }
    , { signature = "awaqeeeeedq", internalName = "display_strikethrough", action = displayStrikethrough, displayName = "Editor's Gambit", outputOptions = [ DisplayType ], selectedOutput = Just ( DisplayType, Display "" "" ), startDirection = Southwest }
    , { signature = "awaqeeeeeda", internalName = "display_obfuscated", action = displayObfuscated, displayName = "Censor's Gambit", outputOptions = [ DisplayType ], selectedOutput = Just ( DisplayType, Display "" "" ), startDirection = Southwest }
    , { signature = "awaqeeeeedaqa", internalName = "display_font", action = displayFont, displayName = "Calligrapher's Gambit", outputOptions = [ DisplayType ], selectedOutput = Just ( DisplayType, Display "" "" ), startDirection = Southwest }
    -- Display manipulation
    , { signature = "dwdeqqqqqdda", internalName = "compare_style", action = compareStyle, displayName = "Stylistic Distillation", outputOptions = [ BooleanType ], selectedOutput = Just ( BooleanType, Boolean False ), startDirection = Southeast }
    , { signature = "dwdewqqqwqqaeq", internalName = "parse_display", action = parseDisplay, displayName = "Calculator Purification", outputOptions = [ NumberType, NullType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = Southeast }
    , { signature = "dwdeqqqwqqqqae", internalName = "split_display", action = splitDisplay, displayName = "Cleaving Distillation", outputOptions = [ IotaListType DisplayType ], selectedOutput = Just ( IotaListType DisplayType, IotaList Array.empty ), startDirection = Southeast }
    , { signature = "dwdeqqqqqdeee", internalName = "disintegrate_display", action = disintegrateDisplay, displayName = "Streaming Purification", outputOptions = [ IotaListType DisplayType ], selectedOutput = Just ( IotaListType DisplayType, IotaList Array.empty ), startDirection = Southeast }
    -- Block state queries
    , { signature = "edeeeee", internalName = "is_block_air", action = isBlockAir, displayName = "Void Purification", outputOptions = [ BooleanType ], selectedOutput = Just ( BooleanType, Boolean False ), startDirection = Northeast }
    , { signature = "eaqqqqqe", internalName = "is_block_replaceable", action = isBlockReplaceable, displayName = "Overwriting Purification", outputOptions = [ BooleanType ], selectedOutput = Just ( BooleanType, Boolean False ), startDirection = Northeast }
    , { signature = "qaqqqqqeeeeedq", internalName = "block_hardness", action = blockHardness, displayName = "Miner's Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = East }
    , { signature = "qaqqqqqewaawaawa", internalName = "block_blast_resistance", action = blockBlastResistance, displayName = "Demoman's Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = East }
    , { signature = "qaqqqqqwadeeed", internalName = "blockstate_rotation", action = blockstateRotation, displayName = "Orientation Purification", outputOptions = [ VectorType, NullType ], selectedOutput = Just ( VectorType, Vector ( 0, 0, 0 ) ), startDirection = East }
    , { signature = "qaqqqqqwaea", internalName = "blockstate_crop", action = blockstateCrop, displayName = "Farmer's Purification", outputOptions = [ NumberType, NullType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = East }
    , { signature = "qaqqqeqqqwqaww", internalName = "get_blockstates", action = getBlockstates, displayName = "Facet Purification", outputOptions = [ IotaListType IdentifierType ], selectedOutput = Just ( IotaListType IdentifierType, IotaList Array.empty ), startDirection = East }
    , { signature = "qaqqqqqeawa", internalName = "query_blockstate", action = queryBlockstate, displayName = "Facet Distillation", outputOptions = [ NullType, NumberType, BooleanType ], selectedOutput = Just ( NullType, Null ), startDirection = East }
    , { signature = "qaqqqqqdaqwqwqa", internalName = "block_slipperiness", action = blockSlipperiness, displayName = "Skating Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = East }
    , { signature = "qwedewqqqqq", internalName = "block_map_color", action = blockMapColor, displayName = "Cartographer's Purif.", outputOptions = [ VectorType ], selectedOutput = Just ( VectorType, Vector ( 0, 0, 0 ) ), startDirection = East }
    -- Chat
    , { signature = "aeeedw", internalName = "get_message", action = getMessage, displayName = "News Reflection", outputOptions = [ DisplayType, DisplayType, NumberType ], selectedOutput = Just ( DisplayType, Display "" "" ), startDirection = Southwest }
    , { signature = "dqqqaw", internalName = "get_message_indexed", action = getMessageIndexed, displayName = "News Disintegration", outputOptions = [ DisplayType, DisplayType, NumberType ], selectedOutput = Just ( DisplayType, Display "" "" ), startDirection = Southeast }
    -- Enchantments
    , { signature = "waqwwqawqwawaw", internalName = "get_enchantments", action = getEnchantments, displayName = "Thaumaturgist's Purif.", outputOptions = [ IotaListType IdentifierType ], selectedOutput = Just ( IotaListType IdentifierType, IotaList Array.empty ), startDirection = West }
    , { signature = "wdewwedwewdwdw", internalName = "get_enchantment_strength", action = getEnchantmentStrength, displayName = "Charm Distillation", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = East }
    , { signature = "waawdedwd", internalName = "enchantment_weight", action = enchantmentWeight, displayName = "Conjuring Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = Northeast }
    , { signature = "aaqqadaqwqa", internalName = "can_item_support_enchantment", action = canItemSupportEnchantment, displayName = "Conjuring Distillation", outputOptions = [ BooleanType ], selectedOutput = Just ( BooleanType, Boolean False ), startDirection = West }
    , { signature = "waqwqaqwaaw", internalName = "enchantment_min_level", action = enchantmentMinLevel, displayName = "Valley Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = West }
    , { signature = "wdewedqwaaw", internalName = "enchantment_max_level", action = enchantmentMaxLevel, displayName = "Peak Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = East }
    , { signature = "aeaqwqaqwaaw", internalName = "is_enchantment_cursed", action = isEnchantmentCursed, displayName = "Curse Purification", outputOptions = [ BooleanType ], selectedOutput = Just ( BooleanType, Boolean False ), startDirection = Northwest }
    , { signature = "aqwqaeaqwddw", internalName = "is_enchantment_treasure", action = isEnchantmentTreasure, displayName = "Fable Purification", outputOptions = [ BooleanType ], selectedOutput = Just ( BooleanType, Boolean False ), startDirection = West }
    -- Entity data
    , { signature = "dwe", internalName = "entity_width", action = entityWidth, displayName = "Caliper's Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = Northwest }
    , { signature = "wqaa", internalName = "theodolite", action = theodolite, displayName = "Theodolite Purif.", outputOptions = [ VectorType ], selectedOutput = Just ( VectorType, Vector ( 0, 0, 0 ) ), startDirection = East }
    , { signature = "wddwaqqwawq", internalName = "get_health", action = getHealth, displayName = "Vitality Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 20 ), startDirection = Southeast }
    , { signature = "wddwwawaeqwawq", internalName = "get_max_health", action = getMaxHealth, displayName = "Fitness Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 20 ), startDirection = Southeast }
    , { signature = "eewdead", internalName = "burning", action = burning, displayName = "Inferno Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = West }
    , { signature = "qqqqwaadq", internalName = "is_wet", action = isWet, displayName = "Enderman's Purif.", outputOptions = [ BooleanType ], selectedOutput = Just ( BooleanType, Boolean False ), startDirection = Southwest }
    , { signature = "wwaade", internalName = "get_air", action = getAir, displayName = "Suffocation Purif.", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 15 ), startDirection = East }
    , { signature = "wwaadee", internalName = "get_max_air", action = getMaxAir, displayName = "Lung Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 15 ), startDirection = East }
    , { signature = "aqaew", internalName = "is_sleeping", action = isSleeping, displayName = "Sloth's Purification", outputOptions = [ BooleanType ], selectedOutput = Just ( BooleanType, Boolean False ), startDirection = Northwest }
    , { signature = "eaq", internalName = "is_sprinting", action = isSprinting, displayName = "Racer's Purification", outputOptions = [ BooleanType ], selectedOutput = Just ( BooleanType, Boolean False ), startDirection = West }
    , { signature = "awaqdwaaw", internalName = "is_baby", action = isBaby, displayName = "Youth Purification", outputOptions = [ BooleanType ], selectedOutput = Just ( BooleanType, Boolean False ), startDirection = Southwest }
    , { signature = "awaaqdqaawa", internalName = "breedable", action = breedable, displayName = "Reproduction Purif.", outputOptions = [ BooleanType, NullType ], selectedOutput = Just ( BooleanType, Boolean False ), startDirection = East }
    , { signature = "eqqedwewew", internalName = "entity_vehicle", action = entityVehicle, displayName = "Vehicle Purification", outputOptions = [ EntityType, NullType ], selectedOutput = Just ( NullType, Null ), startDirection = East }
    , { signature = "qeeqawqwqw", internalName = "entity_passengers", action = entityPassengers, displayName = "Jockey Purification", outputOptions = [ IotaListType EntityType ], selectedOutput = Just ( IotaListType EntityType, IotaList Array.empty ), startDirection = East }
    , { signature = "aqwedewwded", internalName = "angry_at", action = angryAt, displayName = "Fixation Purification", outputOptions = [ EntityType, NullType ], selectedOutput = Just ( NullType, Null ), startDirection = Southwest }
    , { signature = "aqawwqaqwed", internalName = "angry_time", action = angryTime, displayName = "Grudge Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = Northeast }
    , { signature = "qqqwaeqa", internalName = "last_attacker", action = lastAttacker, displayName = "Victim Purification", outputOptions = [ EntityType, NullType ], selectedOutput = Just ( NullType, Null ), startDirection = Northwest }
    , { signature = "deqdweee", internalName = "last_attacked", action = lastAttacked, displayName = "Scar Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = East }
    , { signature = "edeweedw", internalName = "entity_name", action = entityName, displayName = "Name Purification", outputOptions = [ DisplayType ], selectedOutput = Just ( DisplayType, Display "" "" ), startDirection = Southwest }
    , { signature = "qdaqwawqeewde", internalName = "pet_owner", action = petOwner, displayName = "Adoration Purification", outputOptions = [ EntityType, NullType ], selectedOutput = Just ( NullType, Null ), startDirection = West }
    , { signature = "qaedwaa", internalName = "is_monster", action = isMonster, displayName = "Malevolence Purif.", outputOptions = [ BooleanType ], selectedOutput = Just ( BooleanType, Boolean False ), startDirection = Northeast }
    , { signature = "aadedade", internalName = "shooter", action = shooter, displayName = "Shooter Purification", outputOptions = [ EntityType, NullType ], selectedOutput = Just ( NullType, Null ), startDirection = East }
    , { signature = "waawedwdwd", internalName = "absorption_hearts", action = absorptionHearts, displayName = "Absorption Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = Northeast }
    -- Environment
    , { signature = "wawaw", internalName = "env_ambit", action = envAmbit, displayName = "Ambit Purification", outputOptions = [ BooleanType ], selectedOutput = Just ( BooleanType, Boolean True ), startDirection = East }
    , { signature = "waaq", internalName = "env_staff", action = envStaff, displayName = "Staff Reflection", outputOptions = [ BooleanType ], selectedOutput = Just ( BooleanType, Boolean False ), startDirection = Northeast }
    , { signature = "qaqqqwaaq", internalName = "env_offhand", action = envOffhand, displayName = "Dexterity Reflection", outputOptions = [ BooleanType ], selectedOutput = Just ( BooleanType, Boolean False ), startDirection = Northeast }
    , { signature = "waaqwwaqqqqq", internalName = "env_packaged_hex", action = envPackagedHex, displayName = "Device Reflection", outputOptions = [ BooleanType ], selectedOutput = Just ( BooleanType, Boolean False ), startDirection = Northeast }
    , { signature = "waaqdeaqwqae", internalName = "env_circle", action = envCircle, displayName = "Constructed Reflection", outputOptions = [ BooleanType ], selectedOutput = Just ( BooleanType, Boolean False ), startDirection = Northeast }
    -- Food
    , { signature = "qqqadaddw", internalName = "get_player_hunger", action = getPlayerHunger, displayName = "Hunger Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 20 ), startDirection = West }
    , { signature = "qqqadaddq", internalName = "get_player_saturation", action = getPlayerSaturation, displayName = "Stamina Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 5 ), startDirection = West }
    , { signature = "adaqqqddqe", internalName = "get_hunger", action = getHunger, displayName = "Calorie Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = West }
    , { signature = "adaqqqddqw", internalName = "get_saturation", action = getSaturation, displayName = "Satiation Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = West }
    , { signature = "adaqqqddaed", internalName = "is_meat", action = isMeat, displayName = "Flesh Purification", outputOptions = [ BooleanType ], selectedOutput = Just ( BooleanType, Boolean False ), startDirection = West }
    , { signature = "adaqqqddaq", internalName = "is_snack", action = isSnack, displayName = "Dessert Purification", outputOptions = [ BooleanType ], selectedOutput = Just ( BooleanType, Boolean False ), startDirection = West }
    , { signature = "adaqqqdd", internalName = "edible", action = edible, displayName = "Edibility Purification", outputOptions = [ BooleanType ], selectedOutput = Just ( BooleanType, Boolean False ), startDirection = West }
    -- Identifier
    , { signature = "qqqqqe", internalName = "identify", action = identify, displayName = "Detective's Purif.", outputOptions = [ IdentifierType ], selectedOutput = Just ( IdentifierType, Identifier "minecraft:stone" ), startDirection = Northeast }
    , { signature = "edqdeq", internalName = "classify", action = classify, displayName = "Modicum Purif.", outputOptions = [ IdentifierType ], selectedOutput = Just ( IdentifierType, Identifier "hexcasting:number" ), startDirection = West }
    -- Item stack
    , { signature = "edeedq", internalName = "get_stack", action = getStack, displayName = "Item Purification", outputOptions = [ ItemStackType ], selectedOutput = Just ( ItemStackType, ItemStack "minecraft:stone" 1 ), startDirection = West }
    , { signature = "qaqqae", internalName = "create_stack", action = createStack, displayName = "Offer Distillation", outputOptions = [ ItemStackType ], selectedOutput = Just ( ItemStackType, ItemStack "minecraft:stone" 1 ), startDirection = East }
    , { signature = "qaqqqq", internalName = "get_mainhand", action = getMainhand, displayName = "Tool Purification", outputOptions = [ ItemStackType ], selectedOutput = Just ( ItemStackType, ItemStack "minecraft:stick" 1 ), startDirection = Northeast }
    , { signature = "edeeee", internalName = "get_offhand", action = getOffhand, displayName = "Accessory Purification", outputOptions = [ ItemStackType ], selectedOutput = Just ( ItemStackType, ItemStack "minecraft:stick" 1 ), startDirection = Northwest }
    , { signature = "qaqddqeeeeqd", internalName = "get_armor", action = getArmor, displayName = "Aegis Purification", outputOptions = [ IotaListType ItemStackType ], selectedOutput = Just ( IotaListType ItemStackType, IotaList Array.empty ), startDirection = Northeast }
    , { signature = "qaqdqaqdeeewedw", internalName = "get_ender_chest", action = getEnderChest, displayName = "Pocket Reflection", outputOptions = [ IotaListType ItemStackType ], selectedOutput = Just ( IotaListType ItemStackType, IotaList Array.empty ), startDirection = Northeast }
    , { signature = "edeeeeeqdee", internalName = "get_inventory", action = getInventory, displayName = "Cart Purification", outputOptions = [ IotaListType ItemStackType, NullType ], selectedOutput = Just ( IotaListType ItemStackType, IotaList Array.empty ), startDirection = West }
    , { signature = "qaqqqqqeaqq", internalName = "get_block_inventory", action = getBlockInventory, displayName = "Chest Purification", outputOptions = [ IotaListType ItemStackType, NullType ], selectedOutput = Just ( IotaListType ItemStackType, IotaList Array.empty ), startDirection = East }
    , { signature = "qaqqwqqqw", internalName = "count_stack", action = countStack, displayName = "Storage Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 1 ), startDirection = East }
    , { signature = "edeeweeew", internalName = "count_max_stack", action = countMaxStack, displayName = "Warehouse Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 64 ), startDirection = West }
    , { signature = "eeweeewdeq", internalName = "damage_stack", action = damageStack, displayName = "Deterioration Purif.", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = Northeast }
    , { signature = "qqwqqqwaqe", internalName = "damage_max_stack", action = damageMaxStack, displayName = "Fragility Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = Northwest }
    , { signature = "dwaawaqwa", internalName = "item_variant", action = itemVariant, displayName = "Glamour Purification", outputOptions = [ NumberType, NullType ], selectedOutput = Just ( NullType, Null ), startDirection = West }
    , { signature = "dwaawaqwawq", internalName = "item_variant_max", action = itemVariantMax, displayName = "Glamour Purification II", outputOptions = [ NumberType, NullType ], selectedOutput = Just ( NullType, Null ), startDirection = West }
    , { signature = "qwawqwaqea", internalName = "item_name", action = itemName, displayName = "Appellation Purification", outputOptions = [ DisplayType ], selectedOutput = Just ( DisplayType, Display "" "" ), startDirection = Southeast }
    , { signature = "dwewdwedea", internalName = "item_lore", action = itemLore, displayName = "Legacy Purification", outputOptions = [ IotaListType DisplayType ], selectedOutput = Just ( IotaListType DisplayType, IotaList Array.empty ), startDirection = Northwest }
    , { signature = "awqqwaqd", internalName = "read_book", action = readBook, displayName = "Literature Purification", outputOptions = [ IotaListType DisplayType, NullType ], selectedOutput = Just ( IotaListType DisplayType, IotaList Array.empty ), startDirection = West }
    , { signature = "eaedweew", internalName = "book_sources", action = bookSources, displayName = "Bibliography Purif.", outputOptions = [ DisplayType, NumberType ], selectedOutput = Just ( DisplayType, Display "" "" ), startDirection = East }
    , { signature = "wqqed", internalName = "item_rarity", action = itemRarity, displayName = "Collector Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = Northeast }
    -- Media
    , { signature = "dde", internalName = "env_media", action = envMedia, displayName = "Media Reflection", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = West }
    , { signature = "ddew", internalName = "get_media", action = getMedia, displayName = "Media Purification", outputOptions = [ NumberType, NullType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = West }
    , { signature = "ddea", internalName = "get_max_media", action = getMaxMedia, displayName = "Potential Purification", outputOptions = [ NumberType, NullType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = East }
    -- Misc entity
    , { signature = "wqwqqwqwawaaw", internalName = "cat_variant", action = catVariant, displayName = "Feline Purification", outputOptions = [ IdentifierType ], selectedOutput = Just ( IdentifierType, Identifier "minecraft:tabby" ), startDirection = Southwest }
    , { signature = "dedwaqwede", internalName = "creeper_fuse", action = creeperFuse, displayName = "Anger Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = West }
    , { signature = "ewdwewdea", internalName = "get_item_frame_rotation", action = getItemFrameRotation, displayName = "Showcase Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = Northeast }
    , { signature = "awqwawqaa", internalName = "set_item_frame_rotation", action = setItemFrameRotation, displayName = "Showcase Gambit", outputOptions = [], selectedOutput = Nothing, startDirection = Southwest }
    , { signature = "wawwwqwwawwwqadaqeda", internalName = "painting_variant", action = paintingVariant, displayName = "Artistic Purification", outputOptions = [ IdentifierType ], selectedOutput = Just ( IdentifierType, Identifier "minecraft:kebab" ), startDirection = Southwest }
    -- Status effects
    , { signature = "wqqq", internalName = "get_effects_entity", action = getEffectsEntity, displayName = "Diagnosis Purification", outputOptions = [ IotaListType IdentifierType ], selectedOutput = Just ( IotaListType IdentifierType, IotaList Array.empty ), startDirection = Southwest }
    , { signature = "wqqqadee", internalName = "get_effects_item", action = getEffectsItem, displayName = "Prescription Purif.", outputOptions = [ IotaListType IdentifierType ], selectedOutput = Just ( IotaListType IdentifierType, IotaList Array.empty ), startDirection = Southwest }
    , { signature = "wqqqaawd", internalName = "get_effect_category", action = getEffectCategory, displayName = "Condition Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = Southwest }
    , { signature = "wqqqaqwa", internalName = "get_effect_amplifier", action = getEffectAmplifier, displayName = "Concentration Dstl.", outputOptions = [ NumberType, NullType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = Southwest }
    , { signature = "wqqqaqwdd", internalName = "get_effect_duration", action = getEffectDuration, displayName = "Clearance Distillation", outputOptions = [ NumberType, NullType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = Southwest }
    -- Tags
    , { signature = "qaqqqqqwqqd", internalName = "block_tags", action = blockTags, displayName = "Geology Purification", outputOptions = [ IotaListType IdentifierType ], selectedOutput = Just ( IotaListType IdentifierType, IotaList Array.empty ), startDirection = East }
    , { signature = "qaqqqqwqqd", internalName = "entity_tags", action = entityTags, displayName = "Genus Purification", outputOptions = [ IotaListType IdentifierType ], selectedOutput = Just ( IotaListType IdentifierType, IotaList Array.empty ), startDirection = Northeast }
    , { signature = "aqawawqqqd", internalName = "item_tags", action = itemTags, displayName = "Bauble Purification", outputOptions = [ IotaListType IdentifierType ], selectedOutput = Just ( IotaListType IdentifierType, IotaList Array.empty ), startDirection = East }
    -- Villager
    , { signature = "qeqwqwqwqwqeqawdaeaeaeaeaea", internalName = "villager_level", action = villagerLevel, displayName = "Tier Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 1 ), startDirection = East }
    , { signature = "qeqwqwqwqwqeqawewawqwawadeeeee", internalName = "villager_profession", action = villagerProfession, displayName = "Professional Purif.", outputOptions = [ IdentifierType ], selectedOutput = Just ( IdentifierType, Identifier "minecraft:farmer" ), startDirection = East }
    , { signature = "qeqwqwqwqwqeqaweqqqqqwded", internalName = "villager_type", action = villagerType, displayName = "Culture Purification", outputOptions = [ IdentifierType ], selectedOutput = Just ( IdentifierType, Identifier "minecraft:plains" ), startDirection = East }
    , { signature = "qeqwqwqwqwqeqawewwqqwwqwwqqww", internalName = "biome_to_villager", action = biomeToVillager, displayName = "Nurture Purification", outputOptions = [ IdentifierType, NullType ], selectedOutput = Just ( IdentifierType, Identifier "minecraft:plains" ), startDirection = East }
    -- World
    , { signature = "eweweweweweeeaedqdqde", internalName = "get_weather", action = getWeather, displayName = "Meterologist's Refl.", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = West }
    , { signature = "wqwqwqwqwqwaeqqqqaeqaeaeaeaw", internalName = "get_light", action = getLight, displayName = "Luminance Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 15 ), startDirection = Southwest }
    , { signature = "qwqwqwqwqwqqwwaadwdaaww", internalName = "get_power", action = getPower, displayName = "Battery Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = East }
    , { signature = "eweweweweweewwddawaddww", internalName = "get_comparator", action = getComparator, displayName = "Peripheral Purification", outputOptions = [ NumberType, NullType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = West }
    , { signature = "wwawwawwqqawwdwwdwwaqwqwqwqwq", internalName = "get_day", action = getDay, displayName = "Circadian Reflection", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = Southeast }
    , { signature = "wddwaqqwqaddaqqwddwaqqwqaddaq", internalName = "get_time", action = getTime, displayName = "Temporal Reflection", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0 ), startDirection = Southeast }
    , { signature = "qwqwqawdqqaqqdwaqwqwq", internalName = "get_biome", action = getBiome, displayName = "Geographical Purif.", outputOptions = [ IdentifierType ], selectedOutput = Just ( IdentifierType, Identifier "minecraft:plains" ), startDirection = West }
    , { signature = "qwqwqwqwqwqqaedwaqd", internalName = "get_dimension", action = getDimension, displayName = "Plane Reflection", outputOptions = [ IdentifierType ], selectedOutput = Just ( IdentifierType, Identifier "minecraft:overworld" ), startDirection = West }
    , { signature = "eweweweweweeweeedadw", internalName = "get_moon", action = getMoon, displayName = "Lunar Reflection", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 0.5 ), startDirection = West }
    , { signature = "eweweweweweeweeeeewdeee", internalName = "get_slime", action = getSlime, displayName = "Exorcist's Purification", outputOptions = [ BooleanType ], selectedOutput = Just ( BooleanType, Boolean False ), startDirection = West }
    , { signature = "eweweweweweeedaawaqd", internalName = "get_chunk_loaded", action = getChunkLoaded, displayName = "Reality Purification", outputOptions = [ NumberType ], selectedOutput = Just ( NumberType, Number 3 ), startDirection = West }
    , { signature = "aqwawqwqqwqwqwqwqwq", internalName = "get_einstein", action = getEinstein, displayName = "Distortion Reflection", outputOptions = [ BooleanType ], selectedOutput = Just ( BooleanType, Boolean True ), startDirection = Southwest }
    -- Item renaming
    , { signature = "qwawqwaadwa", internalName = "set_item_name", action = setItemName, displayName = "Name Item", outputOptions = [], selectedOutput = Nothing, startDirection = Southeast }
    , { signature = "dwewdweedwa", internalName = "set_item_lore", action = setItemLore, displayName = "Describe Item", outputOptions = [], selectedOutput = Nothing, startDirection = Northwest }
    -- ==================== end hexpose ====================
    --xm1221's Test
    ,{ signature = "adaw", internalName = "average", action = average, displayName = "xm1221's Test", outputOptions = [], selectedOutput = Nothing, startDirection = East}
    ]
        |> List.map
            (\pattern ->
                { signature = pattern.signature
                , startDirection = pattern.startDirection
                , internalName = pattern.internalName
                , action = pattern.action
                , metaAction = None
                , displayName = pattern.displayName
                , outputOptions = pattern.outputOptions
                , selectedOutput = pattern.selectedOutput
                , color = accent1
                , active = True
                }
            )
        |> (++) greatSpellRegistry
        |> (++) metapatternRegistry


metapatternRegistry : List Pattern
metapatternRegistry =
    [ { signature = "qqqq", internalName = "clearPatterns", action = noAction, metaAction = ClearPatterns, displayName = "Clear", outputOptions = [], selectedOutput = Nothing }
    , { signature = "qqqqqa", internalName = "resetApp", action = noAction, metaAction = Reset, displayName = "Reset", outputOptions = [], selectedOutput = Nothing }
    , { signature = "wqa", internalName = "backspace", action = noAction, metaAction = Backspace, displayName = "Backspace", outputOptions = [], selectedOutput = Nothing }
    , { signature = "qwqqqwq", internalName = "wrap", action = noAction, metaAction = Wrap, displayName = "Wrap", outputOptions = [], selectedOutput = Nothing }
    ]
        |> List.map
            (\pattern ->
                { signature = pattern.signature
                , internalName = pattern.internalName
                , action = pattern.action
                , metaAction = pattern.metaAction
                , displayName = pattern.displayName
                , outputOptions = pattern.outputOptions
                , selectedOutput = pattern.selectedOutput
                , color = accent1
                , active = True
                , startDirection = East
                }
            )


numberLiteralGenerator : String -> Bool -> Pattern
numberLiteralGenerator angleSignature isNegative =
    let
        letterMap : Char -> (Float -> Float)
        letterMap letter =
            case letter of
                'w' ->
                    (+) 1

                'q' ->
                    (+) 5

                'e' ->
                    (+) 10

                'a' ->
                    (*) 2

                'd' ->
                    (*) 0.5

                _ ->
                    (+) 0

        numberAbs =
            List.foldl letterMap 0 <| String.toList <| String.dropLeft 4 angleSignature

        number =
            if isNegative then
                -numberAbs

            else
                numberAbs
    in
    { signature = angleSignature
    , action = numberLiteral number
    , metaAction = None
    , displayName = "Numerical Reflection: " ++ String.fromFloat number
    , internalName = String.fromFloat number
    , color = accent1
    , outputOptions = []
    , selectedOutput = Nothing
    , active = True
    , startDirection = Southeast
    }


saveMacro : Array Iota -> CastingContext -> ActionResult
saveMacro stack ctx =
    let
        action iota1 iota2 context =
            case ( iota1, iota2 ) of
                ( value, PatternIota key _ ) ->
                    ( Array.empty
                    , { context
                        | macros =
                            Dict.update key.signature
                                (\val ->
                                    case val of
                                        Just ( displayName, _, _ ) ->
                                            Just ( displayName, key.startDirection, value )

                                        Nothing ->
                                            Just ( "Unnamed Macro", key.startDirection, value )
                                )
                                context.macros
                      }
                    )

                _ ->
                    ( Array.repeat 1 <| Garbage CatastrophicFailure, ctx )

        maybeIota1 =
            Array.get 1 stack

        maybeIota2 =
            Array.get 0 stack

        newStack =
            Array.slice 2 (Array.length stack) stack

        getUnusedPatternIota iota =
            case iota of
                PatternIota pattern _ ->
                    if (getPatternFromSignature Nothing pattern.signature).internalName == "unknown" then
                        Just iota

                    else
                        Nothing

                _ ->
                    Nothing
    in
    if maybeIota1 == Nothing || maybeIota2 == Nothing then
        { stack = Array.append (Array.map mapNothingToMissingIota <| Array.fromList <| moveNothingsToFront [ maybeIota1, maybeIota2 ]) newStack
        , ctx = ctx
        , success = False
        }

    else
        case ( Maybe.map getIotaList maybeIota1, Maybe.map getUnusedPatternIota maybeIota2 ) of
            ( Just iota1, Just iota2 ) ->
                if iota1 == Nothing || iota2 == Nothing then
                    { stack =
                        Array.append
                            (Array.fromList
                                [ Maybe.withDefault (Garbage IncorrectIota) iota1
                                , Maybe.withDefault (Garbage IncorrectIota) iota2
                                ]
                            )
                            newStack
                    , ctx = ctx
                    , success = False
                    }

                else
                    let
                        actionResult =
                            action
                                (Maybe.withDefault (Garbage IncorrectIota) iota1)
                                (Maybe.withDefault (Garbage IncorrectIota) iota2)
                                ctx
                    in
                    if nanOrInfinityCheck (Tuple.first actionResult) then
                        { stack = unshift (Garbage MathematicalError) stack, ctx = Tuple.second actionResult, success = False }

                    else
                        { stack = Array.append (Tuple.first actionResult) newStack, ctx = Tuple.second actionResult, success = True }

            _ ->
                -- this should never happen
                { stack = unshift (Garbage CatastrophicFailure) newStack
                , ctx = ctx
                , success = False
                }
