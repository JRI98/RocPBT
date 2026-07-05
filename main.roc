app [main!] {
	pf: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/0.9/8GdFEvQYS3TeAZxKvTzCLVdQiomweGtXcdZkXNDEeABq.tar.zst",
	random: "https://github.com/kili-ilo/roc-random/releases/download/0.6.0/4mHqd7aiQ1hYkoso9C8JRfnx3GuwcwoDqv8EdqAsLbfN.tar.zst",
}

import random.Random

RandomState := Random.State

RandomFormat := [Default].{
	decode_u8 : RandomFormat, RandomState -> (Try(U8, Str), RandomState)
	decode_u8 = |_fmt, RandomState.(state)| {
		{ value, state: new_state } = Random.step(state, Random.u8)
		(Ok(value), RandomState.(new_state))
	}

	decode_u16 : RandomFormat, RandomState -> (Try(U16, Str), RandomState)
	decode_u16 = |_fmt, RandomState.(state)| {
		{ value, state: new_state } = Random.step(state, Random.u16)
		(Ok(value), RandomState.(new_state))
	}

	decode_u32 : RandomFormat, RandomState -> (Try(U32, Str), RandomState)
	decode_u32 = |_fmt, RandomState.(state)| {
		{ value, state: new_state } = Random.step(state, Random.u32)
		(Ok(value), RandomState.(new_state))
	}

	decode_u64 : RandomFormat, RandomState -> (Try(U64, Str), RandomState)
	decode_u64 = |fmt, rs| {
		match fmt.decode_u32(rs) {
			(Ok(a), rs1) =>
				match fmt.decode_u32(rs1) {
					(Ok(b), rs2) => (Ok(U32.to_u64(a) * 4294967296 + U32.to_u64(b)), rs2)
					(Err(_), rs2) => (Err("decode_u64: fmt.decode_u32(rs1)"), rs2)
				}
			(Err(_), rs1) => (Err("decode_u64: fmt.decode_u32(rs)"), rs1)
		}
	}

	decode_u128 : RandomFormat, RandomState -> (Try(U128, Str), RandomState)
	decode_u128 = |fmt, rs| {
		match fmt.decode_u64(rs) {
			(Ok(a), rs1) =>
				match fmt.decode_u64(rs1) {
					(Ok(b), rs2) => (Ok(U64.to_u128(a) * 18446744073709551616 + U64.to_u128(b)), rs2)
					(Err(_), rs3) => (Err("decode_u128: fmt.decode_u64(rs1)"), rs3)
				}
			(Err(_), rs4) => (Err("decode_u128: fmt.decode_u64(rs)"), rs4)
		}
	}

	decode_i8 : RandomFormat, RandomState -> (Try(I8, Str), RandomState)
	decode_i8 = |_fmt, RandomState.(state)| {
		{ value, state: new_state } = Random.step(state, Random.i8)
		(Ok(value), RandomState.(new_state))
	}

	decode_i16 : RandomFormat, RandomState -> (Try(I16, Str), RandomState)
	decode_i16 = |_fmt, RandomState.(state)| {
		{ value, state: new_state } = Random.step(state, Random.i16)
		(Ok(value), RandomState.(new_state))
	}

	decode_i32 : RandomFormat, RandomState -> (Try(I32, Str), RandomState)
	decode_i32 = |_fmt, RandomState.(state)| {
		{ value, state: new_state } = Random.step(state, Random.i32)
		(Ok(value), RandomState.(new_state))
	}

	decode_i64 : RandomFormat, RandomState -> (Try(I64, Str), RandomState)
	decode_i64 = |fmt, rs| {
		match fmt.decode_u64(rs) {
			(Ok(v), rs1) => (Ok(U64.to_i64_wrap(v)), rs1)
			(Err(_), rs1) => (Err("decode_i64: fmt.decode_u64(rs)"), rs1)
		}
	}

	decode_i128 : RandomFormat, RandomState -> (Try(I128, Str), RandomState)
	decode_i128 = |fmt, rs| {
		match fmt.decode_u128(rs) {
			(Ok(v), rs1) => (Ok(U128.to_i128_wrap(v)), rs1)
			(Err(_), rs1) => (Err("decode_i128: fmt.decode_u128(rs)"), rs1)
		}
	}

	decode_f32 : RandomFormat, RandomState -> (Try(F32, Str), RandomState)
	decode_f32 = |fmt, rs| {
		match fmt.decode_u32(rs) {
			(Ok(bits), rs1) => (Ok(F32.from_bits(bits)), rs1)
			(Err(_), rs1) => (Err("decode_f32: fmt.decode_u32(rs)"), rs1)
		}
	}

	decode_f64 : RandomFormat, RandomState -> (Try(F64, Str), RandomState)
	decode_f64 = |fmt, rs| {
		match fmt.decode_u64(rs) {
			(Ok(bits), rs1) => (Ok(F64.from_bits(bits)), rs1)
			(Err(_), rs1) => (Err("decode_f64: fmt.decode_u64(rs)"), rs1)
		}
	}

	decode_bool : RandomFormat, RandomState -> (Try(Bool, Str), RandomState)
	decode_bool = |fmt, rs| {
		match fmt.decode_u8(rs) {
			(Ok(v), rs1) => (Ok(U8.bitwise_and(v, 1) == 1), rs1)
			(Err(_), rs1) => (Err("decode_bool: fmt.decode_u8(rs)"), rs1)
		}
	}

	decode_list : RandomFormat, RandomState, (RandomState, RandomFormat -> (Try(item, Str), RandomState)) -> (Try(List(item), Str), RandomState)
	decode_list = |fmt, rs, decode_elem| {
		match fmt.decode_u8(rs) {
			(Ok(len_byte), rs1) => {
				list_len = U8.to_u64(len_byte)
				r = Iter.fold(
					0..<list_len,
					{ state: rs1, value: [] },
					|prev, _|
						match decode_elem(prev.state, fmt) {
							(Ok(v), ns) => { state: ns, value: prev.value.append(v) }
							(_, ns) => { state: ns, value: prev.value }
						},
				)
				(Ok(r.value), r.state)
			}
			(Err(_), rs1) => (Err("decode_list: fmt.decode_u8(rs)"), rs1)
		}
	}

	decode_str : RandomFormat, RandomState -> (Try(Str, Str), RandomState)
	decode_str = |fmt, rs| {
		(result, new_state) = fmt.decode_list(rs, |state, f| f.decode_u8(state))
		match result {
			Ok(bytes) => (Ok(Str.from_utf8_lossy(bytes)), new_state)
			Err(_) => (Err("decode_str: fmt.decode_list(rs, decode_u8)"), new_state)
		}
	}

	decode_dec : RandomFormat, RandomState -> (Try(Dec, Str), RandomState)
	decode_dec = |fmt, rs| {
		match fmt.decode_u32(rs) {
			(Ok(hi), rs1) =>
				match fmt.decode_u32(rs1) {
					(Ok(lo), rs2) => (Ok(U32.to_dec(hi) * 4294967296.0 + U32.to_dec(lo)), rs2)
					(Err(_), rs2) => (Err("decode_dec: fmt.decode_u32(rs1)"), rs2)
				}
			(Err(_), rs1) => (Err("decode_dec: fmt.decode_u32(rs)"), rs1)
		}
	}
}

random_value : RandomState -> (a, RandomState) where [a.decode : RandomState, RandomFormat -> (Try(a, Str), RandomState)]
random_value = |rs| {
	Shape : a
	match Shape.decode(rs, RandomFormat.Default) {
		(Ok(value), new_state) => (value, new_state)
		(Err(_), _) => {
			crash "random_value: Shape.decode failed"
		}
	}
}

random_expect : (RandomState -> Bool) -> Bool
random_expect = |fn|
	random_expect_options({ fn, runs: 64 })

random_expect_options : { fn : RandomState -> Bool, runs : U32 } -> Bool
random_expect_options = |opts| {
	var $i = opts.runs
	while $i > 0 {
		if !opts.fn(RandomState.(Random.seed($i))) {
			return False
		}
		$i = $i - 1
	}
	True
}

main! = |_args| {
	Ok({})
}

sum_wrapping : U8, U8 -> U8
sum_wrapping = |a, b| (U8.to_u16(a) + U8.to_u16(b)).to_u8_wrap()

expect
	random_expect(
		|state| {
			(a, state1) = random_value(state)
			(b, _) = random_value(state1)
			sum_wrapping(a, b) == sum_wrapping(b, a)
		},
	)

expect
	random_expect(
		|state| {
			(a, state1) = random_value(state)
			(b, _) = random_value(state1)
			U8.to_u16(a) * U8.to_u16(b) == U8.to_u16(b) * U8.to_u16(a)
		},
	)

expect
	random_expect(
		|state| {
			(a, state1) = random_value(state)
			(b, state2) = random_value(state1)
			(c, _) = random_value(state2)
			(U8.to_u16(a) + U8.to_u16(b)) + U8.to_u16(c) == U8.to_u16(a) + (U8.to_u16(b) + U8.to_u16(c))
		},
	)

expect
	random_expect(
		|state| {
			(a, state1) = random_value(state)
			(b, state2) = random_value(state1)
			(c, _) = random_value(state2)
			U8.to_u32(a) * (U8.to_u32(b) + U8.to_u32(c)) == U8.to_u32(a) * U8.to_u32(b) + U8.to_u32(a) * U8.to_u32(c)
		},
	)

expect
	random_expect(
		|state| {
			(a, state1) = random_value(state)
			(b, _) = random_value(state1)
			U8.bitwise_xor(a, b) == U8.bitwise_xor(b, a)
		},
	)

expect
	random_expect(
		|state| {
			(a, _) = random_value(state)
			U8.bitwise_xor(a, a) == 0
		},
	)

expect
	random_expect(
		|state| {
			(a, state1) = random_value(state)
			(b, state2) = random_value(state1)
			(c, _) = random_value(state2)
			U8.bitwise_xor(U8.bitwise_xor(a, b), c) == U8.bitwise_xor(a, U8.bitwise_xor(b, c))
		},
	)

expect
	random_expect(
		|state| {
			(lst, _) = random_value(state)
			lst_u8 : List(U8)
			lst_u8 = lst
			lst_u8.rev().rev() == lst_u8
		},
	)

expect
	random_expect(
		|state| {
			(lst, _) = random_value(state)
			lst_u8 : List(U8)
			lst_u8 = lst
			List.len(lst_u8.rev()) == List.len(lst_u8)
		},
	)

expect
	random_expect(
		|state| {
			(xs, state1) = random_value(state)
			(ys, _) = random_value(state1)
			xs_u8 : List(U8)
			xs_u8 = xs
			ys_u8 : List(U8)
			ys_u8 = ys
			List.rev(List.concat(xs_u8, ys_u8)) == List.concat(List.rev(ys_u8), List.rev(xs_u8))
		},
	)

expect
	random_expect(
		|state| {
			(xs, state1) = random_value(state)
			(ys, _) = random_value(state1)
			xs_u8 : List(U8)
			xs_u8 = xs
			ys_u8 : List(U8)
			ys_u8 = ys
			List.len(List.concat(xs_u8, ys_u8)) == List.len(xs_u8) + List.len(ys_u8)
		},
	)

expect
	random_expect(
		|state| {
			(lst, _) = random_value(state)
			List.len(List.map(lst, |x| U8.bitwise_and(x, 0xFF))) == List.len(lst)
		},
	)

expect
	random_expect(
		|state| {
			(lst, _) = random_value(state)
			lst_u8 : List(U8)
			lst_u8 = lst
			List.len(List.keep_if(lst_u8, |x| x > 127)) <= List.len(lst_u8)
		},
	)

sorted_u8 : List(U8) -> List(U8)
sorted_u8 = |lst| List.sort_with(lst, |a, b| if a < b LT else if a > b GT else EQ)

expect
	random_expect(
		|state| {
			(lst, _) = random_value(state)
			sorted_u8(sorted_u8(lst)) == sorted_u8(lst)
		},
	)

expect
	random_expect(
		|state| {
			(lst, _) = random_value(state)
			List.len(sorted_u8(lst)) == List.len(lst)
		},
	)

is_sorted_u8s : List(U8) -> Bool
is_sorted_u8s = |lst| {
	zero : U8
	zero = 0
	List.fold_until(
		lst,
		{ sorted: True, prev: zero },
		|s, x|
			if s.sorted and x >= s.prev {
				Continue({ sorted: True, prev: x })
			} else {
				Break({ sorted: False, prev: x })
			},
	).sorted
}

expect
	random_expect(
		|state| {
			(lst, _) = random_value(state)
			is_sorted_u8s(sorted_u8(lst))
		},
	)

all_keys_in : Dict(U8, U8), List(U8) -> Bool
all_keys_in = |dict, keys|
	List.fold_until(
		keys,
		True,
		|_, k|
			match Dict.get(dict, k) {
				Ok(_) => Continue(True)
				_ => Break(False)
			},
	)

pairs_from : RandomState -> { state : RandomState, pairs : List((U8, U8)) }
pairs_from = |state| {
	(keys, state1) = random_value(state)
	(vals, new_state) = random_value(state1)
	{ state: new_state, pairs: List.map2(keys, vals, |k, v| (k, v)) }
}

keys_of : List((U8, U8)) -> List(U8)
keys_of = |pairs| List.map(
	pairs,
	|(k, _)| k,
)

expect
	random_expect(
		|state| {
			{ pairs, .. } = pairs_from(state)
			all_keys_in(Dict.from_list(pairs), keys_of(pairs))
		},
	)

expect
	random_expect(
		|state| {
			r1 = pairs_from(state)
			r2 = pairs_from(r1.state)
			union = Dict.insert_all(Dict.from_list(r1.pairs), Dict.from_list(r2.pairs))
			all_keys_in(union, keys_of(r1.pairs))
				and
				all_keys_in(union, keys_of(r2.pairs))
		},
	)

expect
	random_expect(
		|state| {
			(key, state1) = random_value(state)
			(val, _) = random_value(state1)
			k : U8
			k = key
			v : U8
			v = val
			Dict.get(Dict.single(k, v), k) == Ok(v)
		},
	)

expect
	random_expect(
		|state| {
			{ pairs, .. } = pairs_from(state)
			Dict.len(Dict.from_list(pairs)) <= List.len(pairs)
		},
	)
