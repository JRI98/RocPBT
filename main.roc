app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br",
    random: "https://github.com/lukewilliamboswell/roc-random/releases/download/0.2.1/mJSD8-uN-biRqa6CiqdN4-VJsKXxY8b1eFf6mFTe93A.tar.br",
}

import random.Random

generateU8 : Decoder U8 RandomDecoder
generateU8 = Decode.custom \bytes, @RandomDecoder {} ->
    when bytes is
        [byte, .. as rest] ->
            result = byte * Num.toU8 (Num.powInt 256 0)
            { result: Ok result, rest }

        rest ->
            { result: Err TooShort, rest }

generateU16 : Decoder U16 RandomDecoder
generateU16 = Decode.custom \bytes, @RandomDecoder {} ->
    when bytes is
        [byte1, byte2, .. as rest] ->
            result = Num.toU16 byte1 * (Num.powInt 256 1) + Num.toU16 byte2 * (Num.powInt 256 0)
            { result: Ok result, rest }

        rest ->
            { result: Err TooShort, rest }

generateU32 : Decoder U32 RandomDecoder
generateU32 = Decode.custom \bytes, @RandomDecoder {} ->
    when bytes is
        [byte1, byte2, byte3, byte4, .. as rest] ->
            result = Num.toU32 byte1 * (Num.powInt 256 3) + Num.toU32 byte2 * (Num.powInt 256 2) + Num.toU32 byte3 * (Num.powInt 256 1) + Num.toU32 byte4 * (Num.powInt 256 0)
            { result: Ok result, rest }

        rest ->
            { result: Err TooShort, rest }

generateU64 : Decoder U64 RandomDecoder
generateU64 = Decode.custom \bytes, @RandomDecoder {} ->
    when bytes is
        [byte1, byte2, byte3, byte4, byte5, byte6, byte7, byte8, .. as rest] ->
            result = Num.toU64 byte1 * (Num.powInt 256 7) + Num.toU64 byte2 * (Num.powInt 256 6) + Num.toU64 byte3 * (Num.powInt 256 5) + Num.toU64 byte4 * (Num.powInt 256 4) + Num.toU64 byte5 * (Num.powInt 256 3) + Num.toU64 byte6 * (Num.powInt 256 2) + Num.toU64 byte7 * (Num.powInt 256 1) + Num.toU64 byte8 * (Num.powInt 256 0)
            { result: Ok result, rest }

        rest ->
            { result: Err TooShort, rest }

generateU128 : Decoder U128 RandomDecoder
generateU128 = Decode.custom \bytes, @RandomDecoder {} ->
    when bytes is
        [byte1, byte2, byte3, byte4, byte5, byte6, byte7, byte8, byte9, byte10, byte11, byte12, byte13, byte14, byte15, byte16, .. as rest] ->
            result = Num.toU128 byte1 * (Num.powInt 256 15) + Num.toU128 byte2 * (Num.powInt 256 14) + Num.toU128 byte3 * (Num.powInt 256 13) + Num.toU128 byte4 * (Num.powInt 256 12) + Num.toU128 byte5 * (Num.powInt 256 11) + Num.toU128 byte6 * (Num.powInt 256 10) + Num.toU128 byte7 * (Num.powInt 256 9) + Num.toU128 byte8 * (Num.powInt 256 8) + Num.toU128 byte9 * (Num.powInt 256 7) + Num.toU128 byte10 * (Num.powInt 256 6) + Num.toU128 byte11 * (Num.powInt 256 5) + Num.toU128 byte12 * (Num.powInt 256 4) + Num.toU128 byte13 * (Num.powInt 256 3) + Num.toU128 byte14 * (Num.powInt 256 2) + Num.toU128 byte15 * (Num.powInt 256 1) + Num.toU128 byte16 * (Num.powInt 256 0)
            { result: Ok result, rest }

        rest ->
            { result: Err TooShort, rest }

generateI8 : Decoder I8 RandomDecoder
generateI8 = Decode.custom \bytes, @RandomDecoder {} ->
    when bytes is
        [byte, .. as rest] ->
            resultU8 = byte * Num.toU8 (Num.powInt 256 0)
            result = Num.toI8 resultU8
            { result: Ok result, rest }

        rest ->
            { result: Err TooShort, rest }

generateI16 : Decoder I16 RandomDecoder
generateI16 = Decode.custom \bytes, @RandomDecoder {} ->
    when bytes is
        [byte1, byte2, .. as rest] ->
            resultU16 = Num.toU16 byte1 * (Num.powInt 256 1) + Num.toU16 byte2 * (Num.powInt 256 0)
            result = Num.toI16 resultU16
            { result: Ok result, rest }

        rest ->
            { result: Err TooShort, rest }

generateI32 : Decoder I32 RandomDecoder
generateI32 = Decode.custom \bytes, @RandomDecoder {} ->
    when bytes is
        [byte1, byte2, byte3, byte4, .. as rest] ->
            resultU32 = Num.toU32 byte1 * (Num.powInt 256 3) + Num.toU32 byte2 * (Num.powInt 256 2) + Num.toU32 byte3 * (Num.powInt 256 1) + Num.toU32 byte4 * (Num.powInt 256 0)
            result = Num.toI32 resultU32
            { result: Ok result, rest }

        rest ->
            { result: Err TooShort, rest }

generateI64 : Decoder I64 RandomDecoder
generateI64 = Decode.custom \bytes, @RandomDecoder {} ->
    when bytes is
        [byte1, byte2, byte3, byte4, byte5, byte6, byte7, byte8, .. as rest] ->
            resultU64 = Num.toU64 byte1 * (Num.powInt 256 7) + Num.toU64 byte2 * (Num.powInt 256 6) + Num.toU64 byte3 * (Num.powInt 256 5) + Num.toU64 byte4 * (Num.powInt 256 4) + Num.toU64 byte5 * (Num.powInt 256 3) + Num.toU64 byte6 * (Num.powInt 256 2) + Num.toU64 byte7 * (Num.powInt 256 1) + Num.toU64 byte8 * (Num.powInt 256 0)
            result = Num.toI64 resultU64
            { result: Ok result, rest }

        rest ->
            { result: Err TooShort, rest }

generateI128 : Decoder I128 RandomDecoder
generateI128 = Decode.custom \bytes, @RandomDecoder {} ->
    when bytes is
        [byte1, byte2, byte3, byte4, byte5, byte6, byte7, byte8, byte9, byte10, byte11, byte12, byte13, byte14, byte15, byte16, .. as rest] ->
            resultU128 = Num.toU128 byte1 * (Num.powInt 256 15) + Num.toU128 byte2 * (Num.powInt 256 14) + Num.toU128 byte3 * (Num.powInt 256 13) + Num.toU128 byte4 * (Num.powInt 256 12) + Num.toU128 byte5 * (Num.powInt 256 11) + Num.toU128 byte6 * (Num.powInt 256 10) + Num.toU128 byte7 * (Num.powInt 256 9) + Num.toU128 byte8 * (Num.powInt 256 8) + Num.toU128 byte9 * (Num.powInt 256 7) + Num.toU128 byte10 * (Num.powInt 256 6) + Num.toU128 byte11 * (Num.powInt 256 5) + Num.toU128 byte12 * (Num.powInt 256 4) + Num.toU128 byte13 * (Num.powInt 256 3) + Num.toU128 byte14 * (Num.powInt 256 2) + Num.toU128 byte15 * (Num.powInt 256 1) + Num.toU128 byte16 * (Num.powInt 256 0)
            result = Num.toI128 resultU128
            { result: Ok result, rest }

        rest ->
            { result: Err TooShort, rest }

generateF32 : Decoder F32 RandomDecoder
generateF32 = Decode.custom \bytes, @RandomDecoder {} ->
    when bytes is
        [byteSign, byteExponent, byteFraction1, byteFraction2, byteFraction3, byteFraction4, .. as rest] ->
            sign = Num.bitwiseAnd byteSign 1 == 1
            exponent = byteExponent * Num.toU8 (Num.powInt 256 0)
            fraction = Num.toU32 byteFraction1 * (Num.powInt 256 3) + Num.toU32 byteFraction2 * (Num.powInt 256 2) + Num.toU32 byteFraction3 * (Num.powInt 256 1) + Num.toU32 byteFraction4 * (Num.powInt 256 0)

            result = Num.f32FromParts { sign, exponent, fraction }
            { result: Ok result, rest }

        rest ->
            { result: Err TooShort, rest }

generateF64 : Decoder F64 RandomDecoder
generateF64 = Decode.custom \bytes, @RandomDecoder {} ->
    when bytes is
        [byteSign, byteExponent1, byteExponent2, byteFraction1, byteFraction2, byteFraction3, byteFraction4, byteFraction5, byteFraction6, byteFraction7, byteFraction8, .. as rest] ->
            sign = Num.bitwiseAnd byteSign 1 == 1
            exponent = Num.toU16 byteExponent1 * (Num.powInt 256 1) + Num.toU16 byteExponent2 * (Num.powInt 256 0)
            fraction = Num.toU64 byteFraction1 * (Num.powInt 256 7) + Num.toU64 byteFraction2 * (Num.powInt 256 6) + Num.toU64 byteFraction3 * (Num.powInt 256 5) + Num.toU64 byteFraction4 * (Num.powInt 256 4) + Num.toU64 byteFraction5 * (Num.powInt 256 3) + Num.toU64 byteFraction6 * (Num.powInt 256 2) + Num.toU64 byteFraction7 * (Num.powInt 256 1) + Num.toU64 byteFraction8 * (Num.powInt 256 0)

            result = Num.f64FromParts { sign, exponent, fraction }
            { result: Ok result, rest }

        rest ->
            { result: Err TooShort, rest }

generateDec : Decoder Dec RandomDecoder
generateDec = Decode.custom \bytes, @RandomDecoder {} ->
    when bytes is
        [byte1, byte2, byte3, byte4, byte5, byte6, byte7, byte8, byte9, byte10, byte11, byte12, byte13, byte14, byte15, byte16, .. as rest] ->
            resultU128 = Num.toU128 byte1 * (Num.powInt 256 15) + Num.toU128 byte2 * (Num.powInt 256 14) + Num.toU128 byte3 * (Num.powInt 256 13) + Num.toU128 byte4 * (Num.powInt 256 12) + Num.toU128 byte5 * (Num.powInt 256 11) + Num.toU128 byte6 * (Num.powInt 256 10) + Num.toU128 byte7 * (Num.powInt 256 9) + Num.toU128 byte8 * (Num.powInt 256 8) + Num.toU128 byte9 * (Num.powInt 256 7) + Num.toU128 byte10 * (Num.powInt 256 6) + Num.toU128 byte11 * (Num.powInt 256 5) + Num.toU128 byte12 * (Num.powInt 256 4) + Num.toU128 byte13 * (Num.powInt 256 3) + Num.toU128 byte14 * (Num.powInt 256 2) + Num.toU128 byte15 * (Num.powInt 256 1) + Num.toU128 byte16 * (Num.powInt 256 0)
            result = Num.withDecimalPoint (Num.toI128 resultU128)
            { result: Ok result, rest }

        rest ->
            { result: Err TooShort, rest }

generateBool : Decoder Bool RandomDecoder
generateBool = Decode.custom \bytes, @RandomDecoder {} ->
    when bytes is
        [byte, .. as rest] ->
            bool = Num.bitwiseAnd byte 1 == 1
            { result: Ok bool, rest }

        rest ->
            { result: Err TooShort, rest }

generateString : Decoder Str RandomDecoder
generateString = Decode.custom \bytes, @RandomDecoder {} ->
    when bytes is
        [length, .. as strBytes] ->
            { before, others: rest } = List.split strBytes (Num.toU64 length)

            when Str.fromUtf8 before is
                Ok str ->
                    { result: Ok str, rest }

                Err _ ->
                    { result: Err TooShort, rest }

        rest ->
            { result: Err TooShort, rest }

generateList : Decoder elem RandomDecoder -> Decoder (List elem) RandomDecoder
generateList = \elemDecoder -> Decode.custom \bytes, @RandomDecoder {} ->
        when bytes is
            [length, .. as elemBytes] ->
                { remainingBytes, elements } =
                    List.range { start: At 0, end: Before length }
                    |> List.walkUntil
                        { remainingBytes: elemBytes, elements: Ok [] }
                        (\state, _ ->
                            when state.elements is
                                Ok elems ->
                                    { result, rest } = Decode.decodeWith state.remainingBytes elemDecoder randomDecoder
                                    when result is
                                        Ok value ->
                                            Continue { remainingBytes: rest, elements: Ok (List.append elems value) }

                                        Err _ ->
                                            Break { remainingBytes: rest, elements: Err TooShort }

                                Err _ ->
                                    crash "unreachable"
                        )
                { result: elements, rest: remainingBytes }

            rest ->
                { result: Err TooShort, rest }

generateRecord : state, (state, Str -> [Keep (Decoder state RandomDecoder), Skip]), (state, RandomDecoder -> Result val DecodeError) -> Decoder val RandomDecoder
generateRecord = \_initialState, _stepField, _finalizer -> Decode.custom \_bytes, @RandomDecoder {} ->
        crash "record is not supported"

generateTuple : state, (state, U64 -> [Next (Decoder state RandomDecoder), TooLong]), (state -> Result val DecodeError) -> Decoder val RandomDecoder
generateTuple = \initialState, stepField, finalizer -> Decode.custom \bytes, @RandomDecoder {} ->
        decodeElems = \state, index, elemBytes ->
            when stepField state index is
                Next decoder ->
                    { result: newState, rest: newElemBytes } = Decode.decodeWith elemBytes decoder randomDecoder
                    when newState is
                        Ok ns ->
                            decodeElems ns (index + 1) newElemBytes

                        Err _ ->
                            { result: Err TooShort, rest: newElemBytes }

                TooLong ->
                    { result: Ok state, rest: elemBytes }

        { result: result, rest: rest } = decodeElems initialState 0 bytes

        when result is
            Ok r ->
                when finalizer r is
                    Ok val -> { result: Ok val, rest }
                    Err e -> { result: Err e, rest }

            Err _ -> { result: Err TooShort, rest: rest }

RandomDecoder := {}
    implements [
        DecoderFormatting {
            u8: generateU8,
            u16: generateU16,
            u32: generateU32,
            u64: generateU64,
            u128: generateU128,
            i8: generateI8,
            i16: generateI16,
            i32: generateI32,
            i64: generateI64,
            i128: generateI128,
            f32: generateF32,
            f64: generateF64,
            dec: generateDec,
            bool: generateBool,
            string: generateString,
            list: generateList,
            record: generateRecord,
            tuple: generateTuple,
        },
    ]

randomDecoder = @RandomDecoder {}

RandomState := List U8

randomValue : RandomState -> (a, RandomState) where a implements Decoding
randomValue = \@RandomState bytes ->
    { result, rest } = Decode.fromBytesPartial bytes randomDecoder
    when result is
        Ok value -> (value, @RandomState rest)
        Err err -> crash (Inspect.toStr err)

u32Generator = Random.u32 0 (Num.maxU32 - 1)

randomExpect : { fn : RandomState -> Bool, runs ? U32, bytesMax ? U32 } -> Bool
randomExpect = \{ fn, runs ? 1024, bytesMax ? 1024 } ->
    if runs == 0 then
        Bool.true
    else
        initialSeed = Random.seed32 runs

        result =
            List.range { start: At 0, end: Before (bytesMax) }
            |> List.walk { seed: initialSeed, bytes: [] } \state, _ ->
                random = u32Generator state.seed
                value = Num.toU8 random.value
                { seed: random.state, bytes: List.append state.bytes value }

        fnResult = fn (@RandomState result.bytes)
        if fnResult then
            randomExpect { fn, runs: (runs - 1) }
        else
            Bool.false

sink = \_ -> ""
main = crash (sink randomExpect)

### TESTS ###

# SUM #
sum : U8, U8 -> U8
sum = \a, b -> Num.addWrap a b

expect
    randomExpect {
        fn: \state ->
            ((input1, input2), _) = randomValue state

            # Commutativity
            sum input1 input2 == sum input2 input1,
    }

expect
    randomExpect {
        fn: \state ->
            ((input1, input2, input3), _) = randomValue state

            # Associativity
            sum (sum input1 input2) input3 == sum input1 (sum input2 input3),
    }

expect
    randomExpect {
        fn: \state ->
            (input1, _) = randomValue state

            # Zero
            sum input1 0 == input1,
    }

# LIST REVERSE #
expect
    randomExpect {
        fn: \state ->
            x : (List U8, RandomState)
            x = randomValue state
            (input, _) = x

            List.reverse (List.reverse input) == input,
    }

# DICT FROMLIST #
expect
    randomExpect {
        fn: \state ->
            (input, _) = randomValue state

            dict : Dict U8 U8
            dict = Dict.fromList input

            List.walkUntil
                input
                Bool.true
                (\_, (k, _) ->
                    when Dict.get dict k is
                        Ok _ -> Continue Bool.true
                        _ -> Break Bool.false
                ),
    }

# DICT INSERTALL #
expect
    randomExpect {
        fn: \state ->
            ((input1, input2), _) = randomValue state

            dict1 : Dict U8 U8
            dict1 = Dict.fromList input1

            dict2 : Dict U8 U8
            dict2 = Dict.fromList input2

            dictUnion = Dict.insertAll dict1 dict2

            Dict.walkUntil
                dict1
                Bool.true
                (\_, k, _ ->
                    when Dict.get dictUnion k is
                        Ok _ -> Continue Bool.true
                        _ -> Break Bool.false
                )
            &&
            Dict.walkUntil
                dict2
                Bool.true
                (\_, k, _ ->
                    when Dict.get dictUnion k is
                        Ok _ -> Continue Bool.true
                        _ -> Break Bool.false
                ),
    }
