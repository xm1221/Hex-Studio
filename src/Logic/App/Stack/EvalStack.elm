module Logic.App.Stack.EvalStack exposing (..)

import Array exposing (Array)
import Array.Extra as Array
import Dict
import List.Extra as List
import Logic.App.Patterns.OperatorUtils exposing (getIotaList, getPatternList, getPatternOrIotaList, mapNothingToMissingIota, moveNothingsToFront)
import Logic.App.Types exposing (ActionResult, ApplyToStackResult(..), CastingContext, Iota(..), IotaType(..), Mishap(..), Pattern, Timeline)
import Logic.App.Utils.Utils exposing (isJust, unshift)


type alias ApplyResult =
    { stack : Array Iota
    , resultArray : Array ApplyToStackResult
    , ctx : CastingContext
    , error : Bool
    , halted : Bool
    , timeline : Timeline
    }


applyToStackStopAtErrorOrHalt : Array Iota -> CastingContext -> Array Iota -> ApplyResult
applyToStackStopAtErrorOrHalt stack ctx iotas =
    applyToStackLoop ( stack, Array.empty ) ctx (Array.toList iotas) 0 Array.empty False True


applyPatternsToStack : Array Iota -> CastingContext -> List Pattern -> ApplyResult
applyPatternsToStack stack ctx patterns =
    let
        patternIotas =
            List.map (\pattern -> PatternIota pattern False) patterns

    in
    applyToStackLoop ( stack, Array.empty ) ctx patternIotas 0 Array.empty False False


applyToStackLoop : ( Array Iota, Array ApplyToStackResult ) -> CastingContext -> List Iota -> Int -> Timeline -> Bool -> Bool -> ApplyResult
applyToStackLoop stackResultTuple ctx patterns currentIndex timeline considerThis stopAtErrorOrHalt =
    let
        stack =
            Tuple.first stackResultTuple

        resultArray =
            Tuple.second stackResultTuple

        introspection =
            case Array.get 0 stack of
                Just (OpenParenthesis _) ->
                    True

                _ ->
                    False

        maybeIota =
            case List.head patterns of
                Just (PatternIota pattern considered) ->
                    if pattern.internalName == "constant" then
                        Array.get 0 (pattern.action Array.empty ctx).stack

                    else
                        Just (PatternIota pattern considered)

                head ->
                    head
    in
    case maybeIota of
        Nothing ->
            { stack = stack, resultArray = resultArray, ctx = ctx, error = False, halted = False, timeline = timeline }

        Just (PatternIota pattern _) ->
            if considerThis then
                let
                    applyResult =
                        ( addEscapedIotaToStack stack (PatternIota pattern True), unshift Considered resultArray )
                in
                applyToStackLoop
                    applyResult
                    ctx
                    (Maybe.withDefault [] <| List.tail patterns)
                    (currentIndex + 1)
                    (unshift { stack = Tuple.first applyResult, patternIndex = currentIndex } timeline)
                    False
                    stopAtErrorOrHalt

            else if pattern.internalName == "halt" && stopAtErrorOrHalt then
                { stack = stack, resultArray = resultArray, ctx = ctx, error = False, halted = True, timeline = timeline }

            else
                let
                    applyResult =
                        applyPatternToStack stack ctx pattern currentIndex
                in
                if not stopAtErrorOrHalt || (stopAtErrorOrHalt && applyResult.result /= Failed) then
                    applyToStackLoop
                        ( applyResult.stack, unshift applyResult.result resultArray )
                        applyResult.ctx
                        (Maybe.withDefault [] <| List.tail patterns)
                        (currentIndex + 1)
                        (Array.append applyResult.timeline timeline)
                        applyResult.considerNext
                        stopAtErrorOrHalt

                else
                    { stack = applyResult.stack
                    , resultArray = unshift applyResult.result resultArray
                    , ctx = applyResult.ctx
                    , error = True
                    , halted = False
                    , timeline = unshift { stack = applyResult.stack, patternIndex = currentIndex } timeline
                    }

        Just iota ->
            if considerThis || introspection then
                let
                    applyResult =
                        ( addEscapedIotaToStack stack iota, unshift Considered resultArray )
                in
                applyToStackLoop
                    applyResult
                    ctx
                    (Maybe.withDefault [] <| List.tail patterns)
                    (currentIndex + 1)
                    (unshift { stack = Tuple.first applyResult, patternIndex = currentIndex } timeline)
                    False
                    stopAtErrorOrHalt

            else
                { stack = stack, resultArray = resultArray, ctx = ctx, error = True, halted = False, timeline = timeline }


applyPatternToStack : Array Iota -> CastingContext -> Pattern -> Int -> { stack : Array Iota, result : ApplyToStackResult, ctx : CastingContext, considerNext : Bool, timeline : Timeline }
applyPatternToStack stack ctx pattern index =
    case Array.get 0 stack of
        -- if intro on top of stack
        Just (OpenParenthesis list) ->
            let
                numberOfCloseParen =
                    Array.length
                        (Array.filter
                            (\iota ->
                                case iota of
                                    PatternIota pat False ->
                                        pat.internalName == "close_paren"

                                    _ ->
                                        False
                            )
                            list
                        )

                numberOfOpenParen =
                    (+) 1 <|
                        Array.length
                            (Array.filter
                                (\iota ->
                                    case iota of
                                        PatternIota pat False ->
                                            pat.internalName == "open_paren"

                                        _ ->
                                            False
                                )
                                list
                            )

                addToIntroList =
                    Array.set 0 (OpenParenthesis (Array.push (PatternIota pattern False) list)) stack
            in
            if pattern.internalName == "escape" then
                { stack = stack, result = Succeeded, ctx = ctx, considerNext = True, timeline = Array.fromList [ { stack = stack, patternIndex = index } ] }

            else if pattern.internalName == "close_paren" then
                if pattern.internalName == "close_paren" && (numberOfCloseParen + 1) >= numberOfOpenParen then
                    let
                        newStack =
                            Array.map
                                (\iota ->
                                    case iota of
                                        OpenParenthesis l ->
                                            IotaList l

                                        otherIota ->
                                            otherIota
                                )
                                stack
                    in
                    { stack = newStack
                    , result = Succeeded
                    , ctx = ctx
                    , considerNext = False
                    , timeline = Array.fromList [ { stack = newStack, patternIndex = index } ]
                    }

                else
                    { stack = addToIntroList, result = Considered, ctx = ctx, considerNext = False, timeline = Array.fromList [ { stack = addToIntroList, patternIndex = index } ] }

            else
                { stack = addToIntroList, result = Considered, ctx = ctx, considerNext = False, timeline = Array.fromList [ { stack = addToIntroList, patternIndex = index } ] }

        _ ->
            -- if no intro on top
            if pattern.internalName == "escape" || pattern.internalName == "weak_escape" then
                { stack = stack, result = Succeeded, ctx = ctx, considerNext = True, timeline = Array.fromList [ { stack = stack, patternIndex = index } ] }

            else if pattern.internalName == "close_paren" then
                { stack = unshift (PatternIota pattern False) stack, result = Failed, ctx = ctx, considerNext = False, timeline = Array.fromList [ { stack = stack, patternIndex = index } ] }

            else if pattern.internalName == "eval" then
                --special cases for eval and for_each because they need to return multiple stack states for the timeline
                let
                    actionResult =
                        eval stack ctx
                in
                if actionResult.success == True then
                    { stack = actionResult.stack
                    , result = Succeeded
                    , ctx = actionResult.ctx
                    , considerNext = False
                    , timeline = Array.map (\x -> { stack = x, patternIndex = index }) actionResult.allStackStates
                    }

                else
                    { stack = actionResult.stack
                    , result = Failed
                    , ctx = actionResult.ctx
                    , considerNext = False
                    , timeline = Array.map (\x -> { stack = x, patternIndex = index }) actionResult.allStackStates
                    }

            else if pattern.internalName == "for_each" then
                -- Thoth's Gambit
                let
                    actionResult =
                        forEach stack ctx
                in
                if actionResult.success == True then
                    { stack = actionResult.stack
                    , result = Succeeded
                    , ctx = actionResult.ctx
                    , considerNext = False
                    , timeline = Array.map (\x -> { stack = x, patternIndex = index }) actionResult.allStackStates
                    }

                else
                    { stack = actionResult.stack
                    , result = Failed
                    , ctx = actionResult.ctx
                    , considerNext = False
                    , timeline = Array.map (\x -> { stack = x, patternIndex = index }) actionResult.allStackStates
                    }

            -- hexflow Thoth-like meta-patterns (simulated as for_each)
            else if pattern.internalName == "pure_map" then
                -- Thoth-like: apply code to each data element
                let
                    actionResult =
                        forEach stack ctx
                in
                if actionResult.success == True then
                    { stack = actionResult.stack
                    , result = Succeeded
                    , ctx = actionResult.ctx
                    , considerNext = False
                    , timeline = Array.map (\x -> { stack = x, patternIndex = index }) actionResult.allStackStates
                    }

                else
                    { stack = actionResult.stack
                    , result = Failed
                    , ctx = actionResult.ctx
                    , considerNext = False
                    , timeline = Array.map (\x -> { stack = x, patternIndex = index }) actionResult.allStackStates
                    }

            else if List.member pattern.internalName [ "for_range/cube", "for_range/cube/pure" ] then
                -- Generate 3D cuboid vectors
                let
                    actionResult =
                        cubeRangeFunc stack ctx
                in
                if actionResult.success == True then
                    { stack = actionResult.stack
                    , result = Succeeded
                    , ctx = actionResult.ctx
                    , considerNext = False
                    , timeline = Array.map (\x -> { stack = x, patternIndex = index }) actionResult.allStackStates
                    }

                else
                    { stack = actionResult.stack
                    , result = Failed
                    , ctx = actionResult.ctx
                    , considerNext = False
                    , timeline = Array.map (\x -> { stack = x, patternIndex = index }) actionResult.allStackStates
                    }

            else if List.member pattern.internalName [ "for_range/line", "for_range/line/pure" ] then
                -- Generate points along line
                let
                    actionResult =
                        lineRangeFunc stack ctx
                in
                if actionResult.success == True then
                    { stack = actionResult.stack
                    , result = Succeeded
                    , ctx = actionResult.ctx
                    , considerNext = False
                    , timeline = Array.map (\x -> { stack = x, patternIndex = index }) actionResult.allStackStates
                    }

                else
                    { stack = actionResult.stack
                    , result = Failed
                    , ctx = actionResult.ctx
                    , considerNext = False
                    , timeline = Array.map (\x -> { stack = x, patternIndex = index }) actionResult.allStackStates
                    }

            else if List.member pattern.internalName [ "for_range/floodfill", "for_range/floodfill/pure" ] then
                -- Floodfill needs world access; fallback to forEach
                let
                    actionResult =
                        forEach stack ctx
                in
                if actionResult.success == True then
                    { stack = actionResult.stack
                    , result = Succeeded
                    , ctx = actionResult.ctx
                    , considerNext = False
                    , timeline = Array.map (\x -> { stack = x, patternIndex = index }) actionResult.allStackStates
                    }

                else
                    { stack = actionResult.stack
                    , result = Failed
                    , ctx = actionResult.ctx
                    , considerNext = False
                    , timeline = Array.map (\x -> { stack = x, patternIndex = index }) actionResult.allStackStates
                    }

            else if pattern.internalName == "pure_reduce" then
                -- Fold: data[0]=init, apply code to (accumulator, element) for each data[1..]
                let
                    actionResult =
                        pureReduceFunc stack ctx
                in
                if actionResult.success == True then
                    { stack = actionResult.stack
                    , result = Succeeded
                    , ctx = actionResult.ctx
                    , considerNext = False
                    , timeline = Array.map (\x -> { stack = x, patternIndex = index }) actionResult.allStackStates
                    }

                else
                    { stack = actionResult.stack
                    , result = Failed
                    , ctx = actionResult.ctx
                    , considerNext = False
                    , timeline = Array.map (\x -> { stack = x, patternIndex = index }) actionResult.allStackStates
                    }

            -- call_stack: like eval, evaluate pattern with carried args
            else if pattern.internalName == "call_stack" then
                let
                    actionResult =
                        eval stack ctx
                in
                if actionResult.success == True then
                    { stack = actionResult.stack
                    , result = Succeeded
                    , ctx = actionResult.ctx
                    , considerNext = False
                    , timeline = Array.map (\x -> { stack = x, patternIndex = index }) actionResult.allStackStates
                    }

                else
                    { stack = actionResult.stack
                    , result = Failed
                    , ctx = actionResult.ctx
                    , considerNext = False
                    , timeline = Array.map (\x -> { stack = x, patternIndex = index }) actionResult.allStackStates
                    }

            else
                case Dict.get pattern.signature ctx.macros of
                    Just ( _, _, iota ) ->
                        let
                            actionResult =
                                eval (unshift iota stack) ctx
                        in
                        if actionResult.success == True then
                            { stack = actionResult.stack
                            , result = Succeeded
                            , ctx = actionResult.ctx
                            , considerNext = False
                            , timeline = Array.map (\x -> { stack = x, patternIndex = index }) actionResult.allStackStates
                            }

                        else
                            { stack = actionResult.stack
                            , result = Failed
                            , ctx = actionResult.ctx
                            , considerNext = False
                            , timeline = Array.map (\x -> { stack = x, patternIndex = index }) actionResult.allStackStates
                            }

                    Nothing ->
                        let
                            actionResult =
                                let
                                    preActionResult =
                                        pattern.action stack ctx
                                in
                                if preActionResult.success == True && isJust pattern.selectedOutput then
                                    { preActionResult | stack = unshift (Tuple.second <| Maybe.withDefault ( NullType, Null ) pattern.selectedOutput) preActionResult.stack }

                                else
                                    preActionResult
                        in
                        if actionResult.success == True then
                            { stack = actionResult.stack, result = Succeeded, ctx = actionResult.ctx, considerNext = False, timeline = Array.fromList [ { stack = actionResult.stack, patternIndex = index } ] }

                        else
                            { stack = actionResult.stack, result = Failed, ctx = actionResult.ctx, considerNext = False, timeline = Array.fromList [ { stack = actionResult.stack, patternIndex = index } ] }


addEscapedIotaToStack : Array Iota -> Iota -> Array Iota
addEscapedIotaToStack stack iota =
    case Array.get 0 stack of
        Just (OpenParenthesis list) ->
            Array.set 0 (OpenParenthesis (Array.push iota list)) stack

        _ ->
            unshift iota stack


eval : Array Iota -> CastingContext -> { stack : Array Iota, ctx : CastingContext, success : Bool, allStackStates : Array (Array Iota) }
eval stack ctx =
    let
        maybeIota =
            Array.get 0 stack

        newStack =
            Array.slice 1 (Array.length stack) stack
    in
    case maybeIota of
        Nothing ->
            { stack = unshift (Garbage NotEnoughIotas) newStack, ctx = ctx, success = False, allStackStates = Array.fromList [ unshift (Garbage NotEnoughIotas) newStack ] }

        Just iota ->
            case getPatternOrIotaList <| iota of
                Nothing ->
                    { stack = unshift (Garbage IncorrectIota) newStack, ctx = ctx, success = False, allStackStates = Array.fromList [ unshift (Garbage IncorrectIota) newStack ] }

                _ ->
                    case iota of
                        IotaList list ->
                            let
                                applyResult =
                                    applyToStackStopAtErrorOrHalt
                                        newStack
                                        ctx
                                        list
                            in
                            { stack =
                                Array.filter
                                    (\i ->
                                        case i of
                                            OpenParenthesis _ ->
                                                False

                                            _ ->
                                                True
                                    )
                                    applyResult.stack
                            , ctx = applyResult.ctx
                            , success = not applyResult.error
                            , allStackStates = Array.map (\x -> x.stack) applyResult.timeline
                            }

                        PatternIota pattern _ ->
                            let
                                applyResult =
                                    applyToStackStopAtErrorOrHalt newStack ctx (Array.fromList [ PatternIota pattern False ])
                            in
                            { stack = applyResult.stack, ctx = applyResult.ctx, success = not applyResult.error, allStackStates = Array.map (\x -> x.stack) applyResult.timeline }

                        _ ->
                            { stack = Array.fromList [ Garbage CatastrophicFailure ], ctx = ctx, success = False, allStackStates = Array.fromList [ Array.fromList [ Garbage CatastrophicFailure ] ] }


forEach : Array Iota -> CastingContext -> { stack : Array Iota, ctx : CastingContext, success : Bool, allStackStates : Array (Array Iota) }
forEach stack ctx =
    let
        maybeIota1 =
            Array.get 1 stack

        maybeIota2 =
            Array.get 0 stack

        newStack =
            Array.slice 2 (Array.length stack) stack
    in
    if maybeIota1 == Nothing || maybeIota2 == Nothing then
        let
            newNewStack =
                Array.append (Array.map mapNothingToMissingIota <| Array.fromList <| moveNothingsToFront [ maybeIota1, maybeIota2 ]) newStack
        in
        { stack = newNewStack
        , ctx = ctx
        , success = False
        , allStackStates = Array.fromList [ newNewStack ]
        }

    else
        case ( Maybe.map getIotaList maybeIota1, Maybe.map getIotaList maybeIota2 ) of
            ( Just iota1, Just iota2 ) ->
                if iota1 == Nothing || iota2 == Nothing then
                    let
                        newNewStack =
                            Array.append
                                (Array.fromList
                                    [ Maybe.withDefault (Garbage IncorrectIota) iota1
                                    , Maybe.withDefault (Garbage IncorrectIota) iota2
                                    ]
                                )
                                newStack
                    in
                    { stack = newNewStack
                    , ctx = ctx
                    , success = False
                    , allStackStates = Array.fromList [ newNewStack ]
                    }

                else
                    case ( iota1, iota2 ) of
                        ( Just (IotaList patternList), Just (IotaList iotaList) ) ->
                            let
                                applyResult =
                                    Array.foldl
                                        (\iota accumulator ->
                                            if accumulator.continue == False then
                                                accumulator

                                            else
                                                let
                                                    subApplyResult =
                                                        applyToStackStopAtErrorOrHalt
                                                            (unshift iota newStack)
                                                            accumulator.ctx
                                                            patternList

                                                    thothList =
                                                        case Array.get 0 accumulator.stack of
                                                            Just (IotaList list) ->
                                                                list

                                                            _ ->
                                                                Array.empty

                                                    success =
                                                        if accumulator.success == True && subApplyResult.error then
                                                            False

                                                        else
                                                            accumulator.success
                                                in
                                                { stack = Array.set 0 (IotaList (Array.append thothList (Array.reverse subApplyResult.stack))) accumulator.stack
                                                , ctx = subApplyResult.ctx
                                                , success = success
                                                , continue =
                                                    if not success || subApplyResult.halted then
                                                        False

                                                    else
                                                        True
                                                , allStackStates =
                                                    Array.append
                                                        (unshift (Array.set 0 (IotaList (Array.append thothList (Array.reverse subApplyResult.stack))) accumulator.stack) <|
                                                            Array.map (\x -> x.stack) subApplyResult.timeline
                                                        )
                                                        accumulator.allStackStates
                                                }
                                        )
                                        { stack = unshift (IotaList Array.empty) newStack, ctx = ctx, success = True, continue = True, allStackStates = Array.empty }
                                        iotaList
                            in
                            { stack =
                                Array.filter
                                    (\i ->
                                        case i of
                                            OpenParenthesis _ ->
                                                False

                                            _ ->
                                                True
                                    )
                                    applyResult.stack
                            , ctx = applyResult.ctx
                            , success = applyResult.success
                            , allStackStates = applyResult.allStackStates
                            }

                        _ ->
                            { stack = Array.fromList [ Garbage CatastrophicFailure ], ctx = ctx, success = False, allStackStates = Array.fromList [ Array.fromList [ Garbage CatastrophicFailure ] ] }

            _ ->
                -- this should never happen
                { stack = unshift (Garbage CatastrophicFailure) newStack
                , ctx = ctx
                , success = False
                , allStackStates = Array.fromList [ unshift (Garbage CatastrophicFailure) newStack ]
                }


-- pure_reduce: fold over data list using pattern code
-- Stack: [pattern_list], [data_list]
-- data[0] = initial accumulator, data[1..] = elements to fold over
-- Applies pattern to (accumulator, element) for each element, result becomes new accumulator
pureReduceFunc : Array Iota -> CastingContext -> { stack : Array Iota, ctx : CastingContext, success : Bool, allStackStates : Array (Array Iota) }
pureReduceFunc stack ctx =
    let
        maybeCode = Array.get 1 stack
        maybeData = Array.get 0 stack
        newStack = Array.slice 2 (Array.length stack) stack
    in
    case (maybeCode, maybeData) of
        (Just (IotaList codeList), Just (IotaList dataList)) ->
            if Array.length dataList < 2 then
                -- not enough data for reduce, push data back
                { stack = unshift (IotaList dataList) newStack
                , ctx = ctx
                , success = True
                , allStackStates = Array.fromList [ unshift (IotaList dataList) newStack ]
                }

            else
                let
                    initAcc = Array.get 0 dataList |> Maybe.withDefault Null
                    rest = Array.slice 1 (Array.length dataList) dataList

                    foldStep : Iota -> { acc : Iota, ctx_ : CastingContext, success_ : Bool } -> { acc : Iota, ctx_ : CastingContext, success_ : Bool }
                    foldStep element state =
                        if not state.success_ then
                            state

                        else
                            let
                                applyResult =
                                    applyToStackStopAtErrorOrHalt
                                        (unshift element (unshift state.acc Array.empty))
                                        state.ctx_
                                        codeList
                            in
                            case Array.get 0 applyResult.stack of
                                Just result ->
                                    { acc = result, ctx_ = applyResult.ctx, success_ = not applyResult.error }

                                Nothing ->
                                    { acc = Garbage NotEnoughIotas, ctx_ = applyResult.ctx, success_ = False }
                in
                let
                    result = Array.foldl foldStep { acc = initAcc, ctx_ = ctx, success_ = True } rest
                in
                { stack = unshift result.acc newStack
                , ctx = result.ctx_
                , success = result.success_
                , allStackStates = Array.fromList [ unshift result.acc newStack ]
                }

        _ ->
            { stack = unshift (Garbage CatastrophicFailure) newStack
            , ctx = ctx
            , success = False
            , allStackStates = Array.fromList [ unshift (Garbage CatastrophicFailure) newStack ]
            }


-- ============================================================
-- HexFlow range generation (pure math)
-- ============================================================

-- Helper: count how many top-of-stack items are Numbers (for option detection)
countTopNums : Array Iota -> Int
countTopNums stack =
    if Array.length stack == 0 then
        0

    else
        case Array.get 0 stack of
            Just (Number _) ->
                let next = Array.slice 1 (Array.length stack) stack
                in 1 + countTopNums next

            _ ->
                0


-- for_range/cube: generate 3D cuboid vectors between pos1 and pos2
-- Stack (top=0): ..., [code], pos1, pos2(, option=0)
-- Consumes code + pos1 + pos2 + optional option, pushes IotaList of centers
cubeRangeFunc : Array Iota -> CastingContext -> { stack : Array Iota, ctx : CastingContext, success : Bool, allStackStates : Array (Array Iota) }
cubeRangeFunc stack ctx =
    let
        optNums = countTopNums stack  -- 0 or 1 option numbers
        optOffset = min optNums 1
        codeIdx = 2 + optOffset  -- code is 2+opt from top
        pos1Idx = 1 + optOffset
        pos2Idx = 0 + optOffset
        consumed = 3 + optOffset  -- code + pos1 + pos2 + option

        maybeCode = Array.get codeIdx stack
        maybePos1 = Array.get pos1Idx stack
        maybePos2 = Array.get pos2Idx stack
        maybeOpt  = if optOffset > 0 then Array.get 0 stack else Nothing
        newStack = Array.slice consumed (Array.length stack) stack
    in
    case (maybeCode, maybePos1, maybePos2) of
        (Just _, Just (Vector (x1, y1, z1)), Just (Vector (x2, y2, z2))) ->
            let
                minX = round (min x1 x2)
                maxX = round (max x1 x2)
                minY = round (min y1 y2)
                maxY = round (max y1 y2)
                minZ = round (min z1 z2)
                maxZ = round (max z1 z2)

                centers : List Iota
                centers =
                    List.concatMap (\ix ->
                        List.concatMap (\iy ->
                            List.map (\iz ->
                                Vector (toFloat ix + 0.5, toFloat iy + 0.5, toFloat iz + 0.5)
                            ) (List.range minZ maxZ)
                        ) (List.range minY maxY)
                    ) (List.range minX maxX)

                sorted =
                    case maybeOpt of
                        Just (Number opt) ->
                            if round opt == 3 then List.reverse centers else centers
                        _ -> centers
            in
            { stack = unshift (IotaList (Array.fromList sorted)) newStack
            , ctx = ctx
            , success = True
            , allStackStates = Array.fromList [ unshift (IotaList (Array.fromList sorted)) newStack ]
            }

        _ ->
            { stack = unshift (Garbage IncorrectIota) newStack
            , ctx = ctx
            , success = False
            , allStackStates = Array.fromList [ unshift (Garbage IncorrectIota) newStack ]
            }


-- for_range/line: generate equally spaced points along line pos1→pos2
-- Stack (top=0): ..., [code], pos1, pos2(, option=2, sep)
-- Consumes code + pos1 + pos2 + optional option + optional sep, pushes IotaList
lineRangeFunc : Array Iota -> CastingContext -> { stack : Array Iota, ctx : CastingContext, success : Bool, allStackStates : Array (Array Iota) }
lineRangeFunc stack ctx =
    let
        optNums = countTopNums stack  -- 0, 1, or 2
        optOffset = min optNums 2
        codeIdx = 2 + optOffset
        pos1Idx = 1 + optOffset
        pos2Idx = 0 + optOffset
        consumed = 3 + optOffset

        maybePos1 = Array.get pos1Idx stack
        maybePos2 = Array.get pos2Idx stack
        -- sep: if optOffset==2, it's at index 0; if optOffset==1, option is at 0 and we use default; if 0, default
        maybeSep  = if optOffset == 2 then Array.get 0 stack else Nothing
        newStack = Array.slice consumed (Array.length stack) stack
    in
    case (maybePos1, maybePos2) of
        (Just (Vector (x1, y1, z1)), Just (Vector (x2, y2, z2))) ->
            let
                dx = x2 - x1
                dy = y2 - y1
                dz = z2 - z1

                sep =
                    case maybeSep of
                        Just (Number s) -> round s
                        _ -> ceiling (max (max (abs dx) (abs dy)) (abs dz))

                safeSep = max 1 (min 10000 sep)

                points : List Iota
                points =
                    List.map (\i ->
                        let t = toFloat i / toFloat safeSep
                        in Vector (x1 + dx * t, y1 + dy * t, z1 + dz * t)
                    ) (List.range 0 safeSep)
            in
            { stack = unshift (IotaList (Array.fromList points)) newStack
            , ctx = ctx
            , success = True
            , allStackStates = Array.fromList [ unshift (IotaList (Array.fromList points)) newStack ]
            }

        _ ->
            { stack = unshift (Garbage IncorrectIota) newStack
            , ctx = ctx
            , success = False
            , allStackStates = Array.fromList [ unshift (Garbage IncorrectIota) newStack ]
            }
