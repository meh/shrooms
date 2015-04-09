use std::sync::{Once, ONCE_INIT};
use std::mem::swap;

static TABLES: Once = ONCE_INIT;

pub struct Salt {
	pub chars: [u8; 2],

	sb:   [[i64; 4096]; 4],
	bits: i64,
}

static mut DO_PC1:     [[[i64; 128]; 2]; 8] = [[[0; 128]; 2]; 8];
static mut DO_PC2:     [[i64; 128]; 8]      = [[0; 128]; 8];
static mut EPERM32TAB: [[[i64; 2]; 256]; 4] = [[[0; 2]; 256]; 4];
static mut EFP:        [[[i64; 2]; 64]; 16] = [[[0; 2]; 64]; 16];

pub fn salt(chars: &[u8; 2]) -> Salt {
	const BIT_MASK: [i64; 24] = [
		0x40000000, 0x20000000, 0x10000000, 0x08000000, 0x04000000, 0x02000000,
		0x01000000, 0x00800000, 0x00400000, 0x00200000, 0x00100000, 0x00080000,
		0x00004000, 0x00002000, 0x00001000, 0x00000800, 0x00000400, 0x00000200,
		0x00000100, 0x00000080, 0x00000040, 0x00000020, 0x00000010, 0x00000008
	];

	TABLES.call_once(|| {
		const PC1: [usize; 56] = [
			57, 49, 41, 33, 25, 17,  9,  1, 58, 50, 42, 34, 26, 18,
			10,  2, 59, 51, 43, 35, 27, 19, 11,  3, 60, 52, 44, 36,
			63, 55, 47, 39, 31, 23, 15,  7, 62, 54, 46, 38, 30, 22,
			14,  6, 61, 53, 45, 37, 29, 21, 13,  5, 28, 20, 12,  4
		];

		const PC2: [usize; 48] = [
			14, 17, 11, 24,  1,  5,  3, 28, 15,  6, 21, 10,
			23, 19, 12,  4, 26,  8, 16,  7, 27, 20, 13,  2,
			41, 52, 31, 37, 47, 55, 30, 40, 51, 45, 33, 48,
			44, 49, 39, 56, 34, 53, 46, 42, 50, 36, 29, 32
		];

		const PERM32: [usize; 32] = [
			16,  7, 20, 21, 29, 12, 28, 17,  1, 15, 23, 26,  5, 18, 31, 10,
			2,   8, 24, 14, 32, 27,  3,  9, 19, 13, 30,  6, 22, 11,  4, 25
		];

		const ESEL: [usize; 48] = [
			32,  1,  2,  3,  4,  5,  4,  5,  6,  7,  8,  9,
			 8,  9, 10, 11, 12, 13, 12, 13, 14, 15, 16, 17,
			16, 17, 18, 19, 20, 21, 20, 21, 22, 23, 24, 25,
			24, 25, 26, 27, 28, 29, 28, 29, 30, 31, 32,  1
		];

		const FINAL_PERM: [usize; 64] = [
			40,  8, 48, 16, 56, 24, 64, 32, 39,  7, 47, 15, 55, 23, 63, 31,
			38,  6, 46, 14, 54, 22, 62, 30, 37,  5, 45, 13, 53, 21, 61, 29,
			36,  4, 44, 12, 52, 20, 60, 28, 35,  3, 43, 11, 51, 19, 59, 27,
			34,  2, 42, 10, 50, 18, 58, 26, 33,  1, 41,  9, 49, 17, 57, 25
		];

		const BYTE_MASK: [usize; 8] = [
			0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01
		];

		const LONG_MASK: [i64; 32] = [
			0x80000000, 0x40000000, 0x20000000, 0x10000000,
			0x08000000, 0x04000000, 0x02000000, 0x01000000,
			0x00800000, 0x00400000, 0x00200000, 0x00100000,
			0x00080000, 0x00040000, 0x00020000, 0x00010000,
			0x00008000, 0x00004000, 0x00002000, 0x00001000,
			0x00000800, 0x00000400, 0x00000200, 0x00000100,
			0x00000080, 0x00000040, 0x00000020, 0x00000010,
			0x00000008, 0x00000004, 0x00000002, 0x00000001
		];

		for bit in 0 .. 56 {
			let from_bit  = PC1[bit] - 1;
			let byte_mask = BYTE_MASK[from_bit % 8 + 1];
			let long_mask = LONG_MASK[bit % 28 + 4];

			for j in 0 .. 128 {
				if j & byte_mask != 0 {
					unsafe {
						DO_PC1[from_bit / 8][bit / 28][j] |= long_mask;
					}
				}
			}
		}

		for bit in 0 .. 48 {
			let from_bit  = PC2[bit] - 1;
			let byte_mask = BYTE_MASK[from_bit % 7 + 1];
			let bit_mask  = BIT_MASK[bit % 24];

			for j in 0 .. 128 {
				if j & byte_mask != 0 {
					unsafe {
						DO_PC2[from_bit / 7][j] |= bit_mask;
					}
				}
			}
		}

		for bit in 0 .. 48 {
			let from = PERM32[ESEL[bit] - 1] - 1;
			let mask = BYTE_MASK[from % 8];

			for j in 0 .. 256 {
				if j & mask != 0 {
					unsafe {
						EPERM32TAB[from / 8][j][bit / 24] |= BIT_MASK[bit % 24];
					}
				}
			}
		}

		let mut inverse = [0usize; 64];

		for bit in 0 .. 48 {
			inverse[ESEL[bit] - 1] = bit;
			inverse[ESEL[bit] - 1 + 32] = bit + 48;
		}

		for bit in 0 .. 64 {
			let i_long = bit / 32;
			let i_bit  = bit % 32;

			let from_f_bit  = FINAL_PERM[bit] - 1;
			let from_e_bit  = inverse[from_f_bit];
			let from_word   = from_e_bit / 6;
			let within_word = from_e_bit % 6;

			let mask1 = LONG_MASK[within_word + 26] as usize;
			let mask2 = LONG_MASK[i_bit];

			for j in 0 .. 64 {
				if j & mask1 != 0 {
					unsafe {
						EFP[from_word][j][i_long] |= mask2;
					}
				}
			}
		}
	});

	#[inline(always)]
	fn from_ascii(c: u8) -> u8 {
		if c >= b'a' {
			c - 59
		}
		else if c >= b'A' {
			c - 53
		}
		else {
			c - b'.'
		}
	}

	#[inline(always)]
	fn lookup(i: usize, s: usize) -> usize {
		const SBOX: [[[usize; 16]; 4]; 8] = [
			[ [ 14,  4, 13,  1,  2, 15, 11,  8,  3, 10,  6, 12,  5,  9,  0,  7 ],
					[  0, 15,  7,  4, 14,  2, 13,  1, 10,  6, 12, 11,  9,  5,  3,  8 ],
					[  4,  1, 14,  8, 13,  6,  2, 11, 15, 12,  9,  7,  3, 10,  5,  0 ],
					[ 15, 12,  8,  2,  4,  9,  1,  7,  5, 11,  3, 14, 10,  0,  6, 13 ]
				],

				[ [ 15,  1,  8, 14,  6, 11,  3,  4,  9,  7,  2, 13, 12,  0,  5, 10 ],
					[  3, 13,  4,  7, 15,  2,  8, 14, 12,  0,  1, 10,  6,  9, 11,  5 ],
					[  0, 14,  7, 11, 10,  4, 13,  1,  5,  8, 12,  6,  9,  3,  2, 15 ],
					[ 13,  8, 10,  1,  3, 15,  4,  2, 11,  6,  7, 12,  0,  5, 14,  9 ]
				],

				[ [ 10,  0,  9, 14,  6,  3, 15,  5,  1, 13, 12,  7, 11,  4,  2,  8 ],
					[ 13,  7,  0,  9,  3,  4,  6, 10,  2,  8,  5, 14, 12, 11, 15,  1 ],
					[ 13,  6,  4,  9,  8, 15,  3,  0, 11,  1,  2, 12,  5, 10, 14,  7 ],
					[  1, 10, 13,  0,  6,  9,  8,  7,  4, 15, 14,  3, 11,  5,  2, 12 ]
				],

				[ [  7, 13, 14,  3,  0,  6,  9, 10,  1,  2,  8,  5, 11, 12,  4, 15 ],
					[ 13,  8, 11,  5,  6, 15,  0,  3,  4,  7,  2, 12,  1, 10, 14,  9 ],
					[ 10,  6,  9,  0, 12, 11,  7, 13, 15,  1,  3, 14,  5,  2,  8,  4 ],
					[  3, 15,  0,  6, 10,  1, 13,  8,  9,  4,  5, 11, 12,  7,  2, 14 ]
				],

				[ [  2, 12,  4,  1,  7, 10, 11,  6,  8,  5,  3, 15, 13,  0, 14,  9 ],
					[ 14, 11,  2, 12,  4,  7, 13,  1,  5,  0, 15, 10,  3,  9,  8,  6 ],
					[  4,  2,  1, 11, 10, 13,  7,  8, 15,  9, 12,  5,  6,  3,  0, 14 ],
					[ 11,  8, 12,  7,  1, 14,  2, 13,  6, 15,  0,  9, 10,  4,  5,  3 ]
				],

				[ [ 12,  1, 10, 15,  9,  2,  6,  8,  0, 13,  3,  4, 14,  7,  5, 11 ],
					[ 10, 15,  4,  2,  7, 12,  9,  5,  6,  1, 13, 14,  0, 11,  3,  8 ],
					[  9, 14, 15,  5,  2,  8, 12,  3,  7,  0,  4, 10,  1, 13, 11,  6 ],
					[  4,  3,  2, 12,  9,  5, 15, 10, 11, 14,  1,  7,  6,  0,  8, 13 ]
				],

				[ [  4, 11,  2, 14, 15,  0,  8, 13,  3, 12,  9,  7,  5, 10,  6,  1 ],
					[ 13,  0, 11,  7,  4,  9,  1, 10, 14,  3,  5, 12,  2, 15,  8,  6 ],
					[  1,  4, 11, 13, 12,  3,  7, 14, 10, 15,  6,  8,  0,  5,  9,  2 ],
					[  6, 11, 13,  8,  1,  4, 10,  7,  9,  5,  0, 15, 14,  2,  3, 12 ]
				],

				[ [ 13,  2,  8,  4,  6, 15, 11,  1, 10,  9,  3, 14,  5,  0, 12,  7 ],
					[  1, 15, 13,  8, 10,  3,  7,  4, 12,  5,  6, 11,  0, 14,  9,  2 ],
					[  7, 11,  4,  1,  9, 12, 14,  2,  0,  6, 10, 13, 15,  3,  5,  8 ],
					[  2,  1, 14,  7,  4, 10,  8, 13, 15, 12,  9,  0,  3,  5,  6, 11 ]
				]
		];

		SBOX[i][((s >> 4) & 0x2) | (s & 0x1)][(s >> 1) & 0xf]
	}

	#[inline(always)]
	fn shuffle(table: &mut [i64; 4096], salt: i64) {
		for k in table.iter_mut() {
			let x = ((*k >> 32) ^ *k) & salt;

			*k = *k ^ (x << 32) | x;
		}
	}

	let mut salt = Salt {
		sb:    [[0; 4096]; 4],
		chars: *chars,
		bits:  0,
	};

	for sg in 0 .. 4 {
		for j1 in 0 .. 64 {
			let s1 = lookup(2 * sg, j1);

			for j2 in 0 .. 64 {
				let s2         = lookup(2 * sg + 1, j2);
				let to_permute = ((s1 << 4) | s2) << (24 - 8 * sg);
				let inx = (j1 << 6) | j2;

				unsafe {
					salt.sb[sg][inx] = (EPERM32TAB[0][(to_permute >> 24) & 0xff][0] << 32) |
					                   (EPERM32TAB[0][(to_permute >> 24) & 0xff][1]);

					salt.sb[sg][inx] = (EPERM32TAB[0][(to_permute >> 16) & 0xff][0] << 32) |
					                   (EPERM32TAB[0][(to_permute >> 16) & 0xff][1]);

					salt.sb[sg][inx] = (EPERM32TAB[0][(to_permute >>  8) & 0xff][0] << 32) |
					                   (EPERM32TAB[0][(to_permute >>  8) & 0xff][1]);

					salt.sb[sg][inx] = (EPERM32TAB[0][(to_permute)       & 0xff][0] << 32) |
					                   (EPERM32TAB[0][(to_permute)       & 0xff][1]);
				}
			}
		}
	}

	for i in 0 .. 2 {
		let c = from_ascii(chars[i]);

		for j in 0 .. 6 {
			if (c >> j) & 0x1 != 0 {
				salt.bits |= BIT_MASK[6 * i + j];
			}
		}
	}

	shuffle(&mut salt.sb[0], salt.bits);
	shuffle(&mut salt.sb[1], salt.bits);
	shuffle(&mut salt.sb[2], salt.bits);
	shuffle(&mut salt.sb[3], salt.bits);

	salt
}

pub fn crypt(password: &[u8], salt: &Salt) -> String {
	#[inline(always)]
	fn schedule(password: &[u8]) -> [i64; 16] {
		const ROTS: [usize; 16] = [
  		1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1
		];

		let mut sched = [0i64; 16];
		let mut key   = [0u8; 8];

		key.clone_from_slice(password);

		let mut v1 = 0i64;
		let mut v2 = 0i64;

		for i in 0 .. 8 {
			unsafe {
				v1 |= DO_PC1[i][0][(key[i] & 0x7f) as usize];
				v2 |= DO_PC1[i][1][(key[i] & 0x7f) as usize];
			}
		}

		for i in 0 .. 16 {
			let mut v: i64;

			v1 = (v1 << ROTS[i]) | (v1 >> (28 - ROTS[i]));

			unsafe {
				v  = DO_PC2[0][((v2 >> 21) & 0x7f) as usize];
				v |= DO_PC2[1][((v2 >> 14) & 0x7f) as usize];
				v |= DO_PC2[2][((v2 >>  7) & 0x7f) as usize];
				v |= DO_PC2[3][((v2      ) & 0x7f) as usize];
			}

			v <<= 32;

			v2 = (v2 << ROTS[i]) | (v2 >> (28 - ROTS[i]));

			unsafe {
				v |= DO_PC2[4][((v2 >> 21) & 0x7f) as usize];
				v |= DO_PC2[5][((v2 >> 14) & 0x7f) as usize];
				v |= DO_PC2[6][((v2 >>  7) & 0x7f) as usize];
				v |= DO_PC2[7][((v2      ) & 0x7f) as usize];
			}

			sched[i] = v | 0x0000800000008000;
		}

		sched
	}

	#[inline(always)]
	fn iterations(salt: &Salt, schedule: &[i64; 16]) -> (i64, i64, i64, i64) {
		#[inline(always)]
		fn sba(sb1: &[i64; 4096], sb2: &[i64; 4096], v: i64) -> i64 {
			if v > 4096 * 8 {
				sb1[(v / 8) as usize]
			}
			else {
				sb2[(v / 8) as usize]
			}
		}

		let mut l = 0i64;
		let mut r = 0i64;

		for _ in 0 .. 25 {
			for i in 0 .. 8 {
				let s = schedule[i * 2] ^ r;

				l ^= sba(&salt.sb[2], &salt.sb[3], (s      ) & 0xffff);
				l ^= sba(&salt.sb[2], &salt.sb[3], (s >> 16) & 0xffff);
				l ^= sba(&salt.sb[0], &salt.sb[1], (s >> 32) & 0xffff);
				l ^= sba(&salt.sb[0], &salt.sb[1], (s >> 48)         );

				let s = schedule[i * 2 + 1] ^ l;

				r ^= sba(&salt.sb[2], &salt.sb[3], (s      ) & 0xffff);
				r ^= sba(&salt.sb[2], &salt.sb[3], (s >> 16) & 0xffff);
				r ^= sba(&salt.sb[0], &salt.sb[1], (s >> 32) & 0xffff);
				r ^= sba(&salt.sb[0], &salt.sb[1], (s >> 48)         );
			}

			swap(&mut l, &mut r);
		}

		(l >> 32, l & 0xffffffff, r >> 32, r & 0xffffffff)
	}

	#[inline(always)]
	fn permutation((mut l1, mut l2, mut r1, mut r2): (i64, i64, i64, i64), bits: i64) -> (i64, i64) {
		let x = (l1 ^ l2) & bits;
		l1 ^= x;
		l2 ^= x;

		let x = (r1 ^ r2) & bits;
		r1 ^= x;
		r2 ^= x;

		l1 >>= 3;
		l2 >>= 3;
		r1 >>= 3;
		r2 >>= 3;

		let mut v1 = 0;
		let mut v2 = 0;

		unsafe {
			v1 |= EFP[15][(r2 & 0x3f) as usize][0];
			v2 |= EFP[15][(r2 & 0x3f) as usize][1];

			v1 |= EFP[14][((r2 >> 6) & 0x3f) as usize][0];
			v2 |= EFP[14][( r2       & 0x3f) as usize][1];

			v1 |= EFP[13][((r2 >> 16) & 0x3f) as usize][0];
			v2 |= EFP[13][( r2        & 0x3f) as usize][1];

			v1 |= EFP[12][((r2 >> 22) & 0x3f) as usize][0];
			v2 |= EFP[12][( r2        & 0x3f) as usize][1];
			
			v1 |= EFP[11][(r1 & 0x3f) as usize][0];
			v2 |= EFP[11][(r1 & 0x3f) as usize][1];

			v1 |= EFP[10][((r1 >> 6) & 0x3f) as usize][0];
			v2 |= EFP[10][( r1       & 0x3f) as usize][1];

			v1 |= EFP[9][((r1 >> 16) & 0x3f) as usize][0];
			v2 |= EFP[9][( r1        & 0x3f) as usize][1];

			v1 |= EFP[8][((r1 >> 22) & 0x3f) as usize][0];
			v2 |= EFP[8][( r1        & 0x3f) as usize][1];
			
			v1 |= EFP[7][(l2 & 0x3f) as usize][0];
			v2 |= EFP[7][(l2 & 0x3f) as usize][1];

			v1 |= EFP[6][((l2 >> 6) & 0x3f) as usize][0];
			v2 |= EFP[6][( l2       & 0x3f) as usize][1];

			v1 |= EFP[5][((l2 >> 16) & 0x3f) as usize][0];
			v2 |= EFP[5][( l2        & 0x3f) as usize][1];

			v1 |= EFP[4][((l2 >> 22) & 0x3f) as usize][0];
			v2 |= EFP[4][( l2        & 0x3f) as usize][1];
			
			v1 |= EFP[3][(l1 & 0x3f) as usize][0];
			v2 |= EFP[3][(l1 & 0x3f) as usize][1];

			v1 |= EFP[2][((l1 >> 6) & 0x3f) as usize][0];
			v2 |= EFP[2][( l1       & 0x3f) as usize][1];

			v1 |= EFP[1][((l1 >> 16) & 0x3f) as usize][0];
			v2 |= EFP[1][( l1        & 0x3f) as usize][1];

			v1 |= EFP[0][((l1 >> 22) & 0x3f) as usize][0];
			v2 |= EFP[0][( l1        & 0x3f) as usize][1];

			(v1, v2)
		}
	}

	#[inline(always)]
	fn output((v1, mut v2): (i64, i64), salt: &Salt) -> String {
		#[inline(always)]
		fn to_ascii(c: u8) -> u8 {
			if c >= 38 {
				c - 38 + b'a'
			}
			else if c >= 12 {
				c - 12 + b'A'
			}
			else {
				c + b'.'
			}
		}

		let mut result = String::with_capacity(13);

		result.push(salt.chars[0] as char);
		result.push(salt.chars[1] as char);

		for i in 0 .. 5 {
			result.push(to_ascii(((v1 >> (26 - 6 * i)) & 0x3f) as u8) as char);
		}

		let s = (v2 & 0xf) << 2;
		v2  = (v2 >> 2) | ((v1 & 0x3) << 30);

		for i in 5 .. 10 {
			result.push(to_ascii(((v2 >> 56 - 6 * i) & 0x3f) as u8) as char);
		}

		result.push(to_ascii(s as u8) as char);

		result
	}

	output(permutation(iterations(&salt, &schedule(password)), salt.bits), &salt)
}
