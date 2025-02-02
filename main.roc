app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br",
    random: "https://github.com/lukewilliamboswell/roc-random/releases/download/0.5.0/yDUoWipuyNeJ-euaij4w_ozQCWtxCsywj68H0PlJAdE.tar.br",
}

import random.Random

generate_u8 : Decoder U8 RandomDecoder
generate_u8 = Decode.custom(
    |bytes, @RandomDecoder({})|
        when bytes is
            [byte, .. as rest] ->
                result = byte * Num.to_u8(Num.pow_int(256, 0))
                { result: Ok(result), rest }

            rest ->
                { result: Err(TooShort), rest },
)

generate_u16 : Decoder U16 RandomDecoder
generate_u16 = Decode.custom(
    |bytes, @RandomDecoder({})|
        when bytes is
            [byte1, byte2, .. as rest] ->
                result = Num.to_u16(byte1) * (Num.pow_int(256, 1)) + Num.to_u16(byte2) * (Num.pow_int(256, 0))
                { result: Ok(result), rest }

            rest ->
                { result: Err(TooShort), rest },
)

generate_u32 : Decoder U32 RandomDecoder
generate_u32 = Decode.custom(
    |bytes, @RandomDecoder({})|
        when bytes is
            [byte1, byte2, byte3, byte4, .. as rest] ->
                result = Num.to_u32(byte1) * (Num.pow_int(256, 3)) + Num.to_u32(byte2) * (Num.pow_int(256, 2)) + Num.to_u32(byte3) * (Num.pow_int(256, 1)) + Num.to_u32(byte4) * (Num.pow_int(256, 0))
                { result: Ok(result), rest }

            rest ->
                { result: Err(TooShort), rest },
)

generate_u64 : Decoder U64 RandomDecoder
generate_u64 = Decode.custom(
    |bytes, @RandomDecoder({})|
        when bytes is
            [byte1, byte2, byte3, byte4, byte5, byte6, byte7, byte8, .. as rest] ->
                result = Num.to_u64(byte1) * (Num.pow_int(256, 7)) + Num.to_u64(byte2) * (Num.pow_int(256, 6)) + Num.to_u64(byte3) * (Num.pow_int(256, 5)) + Num.to_u64(byte4) * (Num.pow_int(256, 4)) + Num.to_u64(byte5) * (Num.pow_int(256, 3)) + Num.to_u64(byte6) * (Num.pow_int(256, 2)) + Num.to_u64(byte7) * (Num.pow_int(256, 1)) + Num.to_u64(byte8) * (Num.pow_int(256, 0))
                { result: Ok(result), rest }

            rest ->
                { result: Err(TooShort), rest },
)

generate_u128 : Decoder U128 RandomDecoder
generate_u128 = Decode.custom(
    |bytes, @RandomDecoder({})|
        when bytes is
            [byte1, byte2, byte3, byte4, byte5, byte6, byte7, byte8, byte9, byte10, byte11, byte12, byte13, byte14, byte15, byte16, .. as rest] ->
                result = Num.to_u128(byte1) * (Num.pow_int(256, 15)) + Num.to_u128(byte2) * (Num.pow_int(256, 14)) + Num.to_u128(byte3) * (Num.pow_int(256, 13)) + Num.to_u128(byte4) * (Num.pow_int(256, 12)) + Num.to_u128(byte5) * (Num.pow_int(256, 11)) + Num.to_u128(byte6) * (Num.pow_int(256, 10)) + Num.to_u128(byte7) * (Num.pow_int(256, 9)) + Num.to_u128(byte8) * (Num.pow_int(256, 8)) + Num.to_u128(byte9) * (Num.pow_int(256, 7)) + Num.to_u128(byte10) * (Num.pow_int(256, 6)) + Num.to_u128(byte11) * (Num.pow_int(256, 5)) + Num.to_u128(byte12) * (Num.pow_int(256, 4)) + Num.to_u128(byte13) * (Num.pow_int(256, 3)) + Num.to_u128(byte14) * (Num.pow_int(256, 2)) + Num.to_u128(byte15) * (Num.pow_int(256, 1)) + Num.to_u128(byte16) * (Num.pow_int(256, 0))
                { result: Ok(result), rest }

            rest ->
                { result: Err(TooShort), rest },
)

generate_i8 : Decoder I8 RandomDecoder
generate_i8 = Decode.custom(
    |bytes, @RandomDecoder({})|
        when bytes is
            [byte, .. as rest] ->
                result_u8 = byte * Num.to_u8(Num.pow_int(256, 0))
                result = Num.to_i8(result_u8)
                { result: Ok(result), rest }

            rest ->
                { result: Err(TooShort), rest },
)

generate_i16 : Decoder I16 RandomDecoder
generate_i16 = Decode.custom(
    |bytes, @RandomDecoder({})|
        when bytes is
            [byte1, byte2, .. as rest] ->
                result_u16 = Num.to_u16(byte1) * (Num.pow_int(256, 1)) + Num.to_u16(byte2) * (Num.pow_int(256, 0))
                result = Num.to_i16(result_u16)
                { result: Ok(result), rest }

            rest ->
                { result: Err(TooShort), rest },
)

generate_i32 : Decoder I32 RandomDecoder
generate_i32 = Decode.custom(
    |bytes, @RandomDecoder({})|
        when bytes is
            [byte1, byte2, byte3, byte4, .. as rest] ->
                result_u32 = Num.to_u32(byte1) * (Num.pow_int(256, 3)) + Num.to_u32(byte2) * (Num.pow_int(256, 2)) + Num.to_u32(byte3) * (Num.pow_int(256, 1)) + Num.to_u32(byte4) * (Num.pow_int(256, 0))
                result = Num.to_i32(result_u32)
                { result: Ok(result), rest }

            rest ->
                { result: Err(TooShort), rest },
)

generate_i64 : Decoder I64 RandomDecoder
generate_i64 = Decode.custom(
    |bytes, @RandomDecoder({})|
        when bytes is
            [byte1, byte2, byte3, byte4, byte5, byte6, byte7, byte8, .. as rest] ->
                result_u64 = Num.to_u64(byte1) * (Num.pow_int(256, 7)) + Num.to_u64(byte2) * (Num.pow_int(256, 6)) + Num.to_u64(byte3) * (Num.pow_int(256, 5)) + Num.to_u64(byte4) * (Num.pow_int(256, 4)) + Num.to_u64(byte5) * (Num.pow_int(256, 3)) + Num.to_u64(byte6) * (Num.pow_int(256, 2)) + Num.to_u64(byte7) * (Num.pow_int(256, 1)) + Num.to_u64(byte8) * (Num.pow_int(256, 0))
                result = Num.to_i64(result_u64)
                { result: Ok(result), rest }

            rest ->
                { result: Err(TooShort), rest },
)

generate_i128 : Decoder I128 RandomDecoder
generate_i128 = Decode.custom(
    |bytes, @RandomDecoder({})|
        when bytes is
            [byte1, byte2, byte3, byte4, byte5, byte6, byte7, byte8, byte9, byte10, byte11, byte12, byte13, byte14, byte15, byte16, .. as rest] ->
                result_u128 = Num.to_u128(byte1) * (Num.pow_int(256, 15)) + Num.to_u128(byte2) * (Num.pow_int(256, 14)) + Num.to_u128(byte3) * (Num.pow_int(256, 13)) + Num.to_u128(byte4) * (Num.pow_int(256, 12)) + Num.to_u128(byte5) * (Num.pow_int(256, 11)) + Num.to_u128(byte6) * (Num.pow_int(256, 10)) + Num.to_u128(byte7) * (Num.pow_int(256, 9)) + Num.to_u128(byte8) * (Num.pow_int(256, 8)) + Num.to_u128(byte9) * (Num.pow_int(256, 7)) + Num.to_u128(byte10) * (Num.pow_int(256, 6)) + Num.to_u128(byte11) * (Num.pow_int(256, 5)) + Num.to_u128(byte12) * (Num.pow_int(256, 4)) + Num.to_u128(byte13) * (Num.pow_int(256, 3)) + Num.to_u128(byte14) * (Num.pow_int(256, 2)) + Num.to_u128(byte15) * (Num.pow_int(256, 1)) + Num.to_u128(byte16) * (Num.pow_int(256, 0))
                result = Num.to_i128(result_u128)
                { result: Ok(result), rest }

            rest ->
                { result: Err(TooShort), rest },
)

generate_f32 : Decoder F32 RandomDecoder
generate_f32 = Decode.custom(
    |bytes, @RandomDecoder({})|
        when bytes is
            [byte_sign, byte_exponent, byte_fraction1, byte_fraction2, byte_fraction3, byte_fraction4, .. as rest] ->
                sign = Num.bitwise_and(byte_sign, 1) == 1
                exponent = byte_exponent * Num.to_u8(Num.pow_int(256, 0))
                fraction = Num.to_u32(byte_fraction1) * (Num.pow_int(256, 3)) + Num.to_u32(byte_fraction2) * (Num.pow_int(256, 2)) + Num.to_u32(byte_fraction3) * (Num.pow_int(256, 1)) + Num.to_u32(byte_fraction4) * (Num.pow_int(256, 0))

                result = Num.f32_from_parts({ sign, exponent, fraction })
                { result: Ok(result), rest }

            rest ->
                { result: Err(TooShort), rest },
)

generate_f64 : Decoder F64 RandomDecoder
generate_f64 = Decode.custom(
    |bytes, @RandomDecoder({})|
        when bytes is
            [byte_sign, byte_exponent1, byte_exponent2, byte_fraction1, byte_fraction2, byte_fraction3, byte_fraction4, byte_fraction5, byte_fraction6, byte_fraction7, byte_fraction8, .. as rest] ->
                sign = Num.bitwise_and(byte_sign, 1) == 1
                exponent = Num.to_u16(byte_exponent1) * (Num.pow_int(256, 1)) + Num.to_u16(byte_exponent2) * (Num.pow_int(256, 0))
                fraction = Num.to_u64(byte_fraction1) * (Num.pow_int(256, 7)) + Num.to_u64(byte_fraction2) * (Num.pow_int(256, 6)) + Num.to_u64(byte_fraction3) * (Num.pow_int(256, 5)) + Num.to_u64(byte_fraction4) * (Num.pow_int(256, 4)) + Num.to_u64(byte_fraction5) * (Num.pow_int(256, 3)) + Num.to_u64(byte_fraction6) * (Num.pow_int(256, 2)) + Num.to_u64(byte_fraction7) * (Num.pow_int(256, 1)) + Num.to_u64(byte_fraction8) * (Num.pow_int(256, 0))

                result = Num.f64_from_parts({ sign, exponent, fraction })
                { result: Ok(result), rest }

            rest ->
                { result: Err(TooShort), rest },
)

generate_dec : Decoder Dec RandomDecoder
generate_dec = Decode.custom(
    |bytes, @RandomDecoder({})|
        when bytes is
            [byte1, byte2, byte3, byte4, byte5, byte6, byte7, byte8, byte9, byte10, byte11, byte12, byte13, byte14, byte15, byte16, .. as rest] ->
                result_u128 = Num.to_u128(byte1) * (Num.pow_int(256, 15)) + Num.to_u128(byte2) * (Num.pow_int(256, 14)) + Num.to_u128(byte3) * (Num.pow_int(256, 13)) + Num.to_u128(byte4) * (Num.pow_int(256, 12)) + Num.to_u128(byte5) * (Num.pow_int(256, 11)) + Num.to_u128(byte6) * (Num.pow_int(256, 10)) + Num.to_u128(byte7) * (Num.pow_int(256, 9)) + Num.to_u128(byte8) * (Num.pow_int(256, 8)) + Num.to_u128(byte9) * (Num.pow_int(256, 7)) + Num.to_u128(byte10) * (Num.pow_int(256, 6)) + Num.to_u128(byte11) * (Num.pow_int(256, 5)) + Num.to_u128(byte12) * (Num.pow_int(256, 4)) + Num.to_u128(byte13) * (Num.pow_int(256, 3)) + Num.to_u128(byte14) * (Num.pow_int(256, 2)) + Num.to_u128(byte15) * (Num.pow_int(256, 1)) + Num.to_u128(byte16) * (Num.pow_int(256, 0))
                result = Num.with_decimal_point(Num.to_i128(result_u128))
                { result: Ok(result), rest }

            rest ->
                { result: Err(TooShort), rest },
)

generate_bool : Decoder Bool RandomDecoder
generate_bool = Decode.custom(
    |bytes, @RandomDecoder({})|
        when bytes is
            [byte, .. as rest] ->
                bool = Num.bitwise_and(byte, 1) == 1
                { result: Ok(bool), rest }

            rest ->
                { result: Err(TooShort), rest },
)

generate_string : Decoder Str RandomDecoder
generate_string = Decode.custom(
    |bytes, @RandomDecoder({})|
        when bytes is
            [length, .. as str_bytes] ->
                { before, others: rest } = List.split_at(str_bytes, Num.to_u64(length))

                when Str.from_utf8(before) is
                    Ok(str) ->
                        { result: Ok(str), rest }

                    Err(_) ->
                        { result: Err(TooShort), rest }

            rest ->
                { result: Err(TooShort), rest },
)

generate_list : Decoder elem RandomDecoder -> Decoder (List elem) RandomDecoder
generate_list = |elem_decoder|
    Decode.custom(
        |bytes, @RandomDecoder({})|
            when bytes is
                [length, .. as elem_bytes] ->
                    { remaining_bytes, elements } =
                        List.range({ start: At(0), end: Before(length) })
                        |> List.walk_until(
                            { remaining_bytes: elem_bytes, elements: Ok([]) },
                            |state, _|
                                when state.elements is
                                    Ok(elems) ->
                                        { result, rest } = Decode.decode_with(state.remaining_bytes, elem_decoder, random_decoder)
                                        when result is
                                            Ok(value) ->
                                                Continue({ remaining_bytes: rest, elements: Ok(List.append(elems, value)) })

                                            Err(_) ->
                                                Break({ remaining_bytes: rest, elements: Err(TooShort) })

                                    Err(_) ->
                                        crash("unreachable"),
                        )
                    { result: elements, rest: remaining_bytes }

                rest ->
                    { result: Err(TooShort), rest },
    )

generate_record : state, (state, Str -> [Keep (Decoder state RandomDecoder), Skip]), (state, RandomDecoder -> Result val DecodeError) -> Decoder val RandomDecoder
generate_record = |_initialState, _stepField, _finalizer|
    Decode.custom(
        |_bytes, @RandomDecoder({})|
            crash("record is not supported"),
    )

generate_tuple : state, (state, U64 -> [Next (Decoder state RandomDecoder), TooLong]), (state -> Result val DecodeError) -> Decoder val RandomDecoder
generate_tuple = |initial_state, step_field, finalizer|
    Decode.custom(
        |bytes, @RandomDecoder({})|
            decode_elems = |state, index, elem_bytes|
                when step_field(state, index) is
                    Next(decoder) ->
                        { result: new_state, rest: new_elem_bytes } = Decode.decode_with(elem_bytes, decoder, random_decoder)
                        when new_state is
                            Ok(ns) ->
                                decode_elems(ns, (index + 1), new_elem_bytes)

                            Err(_) ->
                                { result: Err(TooShort), rest: new_elem_bytes }

                    TooLong ->
                        { result: Ok(state), rest: elem_bytes }

            { result: result, rest: rest } = decode_elems(initial_state, 0, bytes)

            when result is
                Ok(r) ->
                    when finalizer(r) is
                        Ok(val) -> { result: Ok(val), rest }
                        Err(e) -> { result: Err(e), rest }

                Err(_) -> { result: Err(TooShort), rest: rest },
    )

RandomDecoder := {}
    implements [
        DecoderFormatting {
            u8: generate_u8,
            u16: generate_u16,
            u32: generate_u32,
            u64: generate_u64,
            u128: generate_u128,
            i8: generate_i8,
            i16: generate_i16,
            i32: generate_i32,
            i64: generate_i64,
            i128: generate_i128,
            f32: generate_f32,
            f64: generate_f64,
            dec: generate_dec,
            bool: generate_bool,
            string: generate_string,
            list: generate_list,
            record: generate_record,
            tuple: generate_tuple,
        },
    ]

random_decoder = @RandomDecoder({})

RandomState := List U8

random_value : RandomState -> (a, RandomState) where a implements Decoding
random_value = |@RandomState(bytes)|
    { result, rest } = Decode.from_bytes_partial(bytes, random_decoder)
    when result is
        Ok(value) -> (value, @RandomState(rest))
        Err(err) -> crash(Inspect.to_str(err))

random_expect : (RandomState -> Bool) -> Bool
random_expect = |fn| random_expect_options({ fn, runs: 1024, bytes_max: 1024 })

random_expect_options : { fn : RandomState -> Bool, runs : U32, bytes_max : U32 } -> Bool
random_expect_options = |{ fn, runs, bytes_max }|
    if runs == 0 then
        Bool.true
    else
        initial_seed = Random.seed(runs)

        result =
            List.range({ start: At(0), end: Before(bytes_max) })
            |> List.walk(
                { seed: initial_seed, bytes: [] },
                |state, _|
                    random_generation = Random.u32(state.seed)
                    value = Num.to_u8(random_generation.value)
                    { seed: random_generation.state, bytes: List.append(state.bytes, value) },
            )

        fn_result = fn(@RandomState(result.bytes))
        if fn_result then
            random_expect_options({ fn, runs: (runs - 1), bytes_max })
        else
            Bool.false

sink = |_| ""
main! = crash(sink(random_expect))

### TESTS ###

# SUM #
sum : U8, U8 -> U8
sum = |a, b| Num.add_wrap(a, b)

expect
    random_expect(
        |state|
            ((input1, input2), _) = random_value(state)

            # Commutativity
            sum(input1, input2) == sum(input2, input1),
    )

expect
    random_expect(
        |state|
            ((input1, input2, input3), _) = random_value(state)

            # Associativity
            sum(sum(input1, input2), input3) == sum(input1, sum(input2, input3)),
    )

expect
    random_expect(
        |state|
            (input1, _) = random_value(state)

            # Zero
            sum(input1, 0) == input1,
    )

# LIST REVERSE #
expect
    random_expect(
        |state|
            x : (List U8, RandomState)
            x = random_value(state)
            (input, _) = x

            List.reverse(List.reverse(input)) == input,
    )

# DICT FROM_LIST #
expect
    random_expect(
        |state|
            (input, _) = random_value(state)

            dict : Dict U8 U8
            dict = Dict.from_list(input)

            List.walk_until(
                input,
                Bool.true,
                |_, (k, _)|
                    when Dict.get(dict, k) is
                        Ok(_) -> Continue(Bool.true)
                        _ -> Break(Bool.false),
            ),
    )

# DICT INSERT_ALL #
expect
    random_expect(
        |state|
            ((input1, input2), _) = random_value(state)

            dict1 : Dict U8 U8
            dict1 = Dict.from_list(input1)

            dict2 : Dict U8 U8
            dict2 = Dict.from_list(input2)

            dict_union = Dict.insert_all(dict1, dict2)

            Dict.walk_until(
                dict1,
                Bool.true,
                |_, k, _|
                    when Dict.get(dict_union, k) is
                        Ok(_) -> Continue(Bool.true)
                        _ -> Break(Bool.false),
            )
            and
            Dict.walk_until(
                dict2,
                Bool.true,
                |_, k, _|
                    when Dict.get(dict_union, k) is
                        Ok(_) -> Continue(Bool.true)
                        _ -> Break(Bool.false),
            ),
    )
