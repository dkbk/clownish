/*
  Clownish, a USI shogi(japanese-chess) playing engine derived from Stockfish 7
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad (Stockfish author)
  Copyright (C) 2015-2016 Marco Costalba, Joona Kiiski, Gary Linscott, Tord Romstad (Stockfish author)
  Copyright (C) 2016      dkbk

  Clownish is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Clownish is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef TYPES_H_INCLUDED
#define TYPES_H_INCLUDED

/// When compiling with provided Makefile (e.g. for Linux and OSX), configuration
/// is done automatically. To get started type 'make help'.
///
/// When Makefile is not used (e.g. with Microsoft Visual Studio) some switches
/// need to be set manually:
///
/// -DNDEBUG      | Disable debugging mode. Always use this for release.
///
/// -DNO_PREFETCH | Disable use of prefetch asm-instruction. You may need this to
///               | run on some very old machines.
///
/// -DUSE_POPCNT  | Add runtime support for use of popcnt asm-instruction. Works
///               | only in 64-bit mode and requires hardware with popcnt support.
///
/// -DUSE_PEXT    | Add runtime support for use of pext asm-instruction. Works
///               | only in 64-bit mode and requires hardware with pext support.

#include <cassert>
#include <cctype>
#include <climits>
#include <cstdint>
#include <cstdlib>

#if defined(_MSC_VER)
// Disable some silly and noisy warning from MSVC compiler
#pragma warning(disable: 4127) // Conditional expression is constant
#pragma warning(disable: 4146) // Unary minus operator applied to unsigned type
#pragma warning(disable: 4800) // Forcing value to bool 'true' or 'false'
#endif

/// Predefined macros hell:
///
/// __GNUC__           Compiler is gcc, Clang or Intel on Linux
/// __INTEL_COMPILER   Compiler is Intel
/// _MSC_VER           Compiler is MSVC or Intel on Windows
/// _WIN32             Building on Windows (any)
/// _WIN64             Building on Windows 64 bit

#if defined(_WIN64) && defined(_MSC_VER) // No Makefile used
#  include <intrin.h> // MSVC popcnt and bsfq instrinsics
#  define IS_64BIT
#  define USE_BSFQ
#endif

#if defined(USE_POPCNT) && defined(__INTEL_COMPILER) && defined(_MSC_VER)
#  include <nmmintrin.h> // Intel header for _mm_popcnt_u64() intrinsic
#endif

#if !defined(NO_PREFETCH) && (defined(__INTEL_COMPILER) || defined(_MSC_VER))
#  include <xmmintrin.h> // Intel and Microsoft header for _mm_prefetch()
#endif

#if defined(USE_PEXT)
#  include <immintrin.h> // Header for _pext_u64() intrinsic
#  define pext(b, m) _pext_u64(b, m)
#else
#  define pext(b, m) (0)
#endif

#ifdef USE_POPCNT
const bool HasPopCnt = true;
#else
const bool HasPopCnt = false;
#endif

#ifdef USE_PEXT
const bool HasPext = true;
#else
const bool HasPext = false;
#endif

#ifdef IS_64BIT
const bool Is64Bit = true;
#else
const bool Is64Bit = false;
#endif

#if !defined(CLOWNISH)

typedef uint64_t Key;
typedef uint64_t Bitboard;

const int MAX_MOVES = 256;
const int MAX_PLY   = 128;

/// A move needs 16 bits to be stored
///
/// bit  0- 5: destination square (from 0 to 63)
/// bit  6-11: origin square (from 0 to 63)
/// bit 12-13: promotion piece type - 2 (from KNIGHT-2 to QUEEN-2)
/// bit 14-15: special move flag: promotion (1), en passant (2), castling (3)
/// NOTE: EN-PASSANT bit is set only when a pawn can be captured
///
/// Special cases are MOVE_NONE and MOVE_NULL. We can sneak these in because in
/// any normal move destination square is always different from origin square
/// while MOVE_NONE and MOVE_NULL have the same origin and destination square.

enum Move {
  MOVE_NONE,
  MOVE_NULL = 65
};

enum MoveType {
  NORMAL,
  PROMOTION = 1 << 14,
  ENPASSANT = 2 << 14,
  CASTLING  = 3 << 14
};

enum Color {
  WHITE, BLACK, NO_COLOR, COLOR_NB = 2
};

enum CastlingSide {
  KING_SIDE, QUEEN_SIDE, CASTLING_SIDE_NB = 2
};

enum CastlingRight {
  NO_CASTLING,
  WHITE_OO,
  WHITE_OOO = WHITE_OO << 1,
  BLACK_OO  = WHITE_OO << 2,
  BLACK_OOO = WHITE_OO << 3,
  ANY_CASTLING = WHITE_OO | WHITE_OOO | BLACK_OO | BLACK_OOO,
  CASTLING_RIGHT_NB = 16
};

template<Color C, CastlingSide S> struct MakeCastling {
  static const CastlingRight
  right = C == WHITE ? S == QUEEN_SIDE ? WHITE_OOO : WHITE_OO
                     : S == QUEEN_SIDE ? BLACK_OOO : BLACK_OO;
};

enum Phase {
  PHASE_ENDGAME,
  PHASE_MIDGAME = 128,
  MG = 0, EG = 1, PHASE_NB = 2
};

enum ScaleFactor {
  SCALE_FACTOR_DRAW    = 0,
  SCALE_FACTOR_ONEPAWN = 48,
  SCALE_FACTOR_NORMAL  = 64,
  SCALE_FACTOR_MAX     = 128,
  SCALE_FACTOR_NONE    = 255
};

enum Bound {
  BOUND_NONE,
  BOUND_UPPER,
  BOUND_LOWER,
  BOUND_EXACT = BOUND_UPPER | BOUND_LOWER
};

enum Value : int {
  VALUE_ZERO      = 0,
  VALUE_DRAW      = 0,
  VALUE_KNOWN_WIN = 10000,
  VALUE_MATE      = 32000,
  VALUE_INFINITE  = 32001,
  VALUE_NONE      = 32002,

  VALUE_MATE_IN_MAX_PLY  =  VALUE_MATE - 2 * MAX_PLY,
  VALUE_MATED_IN_MAX_PLY = -VALUE_MATE + 2 * MAX_PLY,

  PawnValueMg   = 198,   PawnValueEg   = 258,
  KnightValueMg = 817,   KnightValueEg = 846,
  BishopValueMg = 836,   BishopValueEg = 857,
  RookValueMg   = 1270,  RookValueEg   = 1281,
  QueenValueMg  = 2521,  QueenValueEg  = 2558,

  MidgameLimit  = 15581, EndgameLimit  = 3998
};

enum PieceType {
  NO_PIECE_TYPE, PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING,
  ALL_PIECES = 0,
  PIECE_TYPE_NB = 8
};

enum Piece {
  NO_PIECE,
  W_PAWN = 1, W_KNIGHT, W_BISHOP, W_ROOK, W_QUEEN, W_KING,
  B_PAWN = 9, B_KNIGHT, B_BISHOP, B_ROOK, B_QUEEN, B_KING,
  PIECE_NB = 16
};

enum Depth {

  ONE_PLY = 1,

  DEPTH_ZERO          =  0,
  DEPTH_QS_CHECKS     =  0,
  DEPTH_QS_NO_CHECKS  = -1,
  DEPTH_QS_RECAPTURES = -5,

  DEPTH_NONE = -6,
  DEPTH_MAX  = MAX_PLY
};

enum Square {
  SQ_A1, SQ_B1, SQ_C1, SQ_D1, SQ_E1, SQ_F1, SQ_G1, SQ_H1,
  SQ_A2, SQ_B2, SQ_C2, SQ_D2, SQ_E2, SQ_F2, SQ_G2, SQ_H2,
  SQ_A3, SQ_B3, SQ_C3, SQ_D3, SQ_E3, SQ_F3, SQ_G3, SQ_H3,
  SQ_A4, SQ_B4, SQ_C4, SQ_D4, SQ_E4, SQ_F4, SQ_G4, SQ_H4,
  SQ_A5, SQ_B5, SQ_C5, SQ_D5, SQ_E5, SQ_F5, SQ_G5, SQ_H5,
  SQ_A6, SQ_B6, SQ_C6, SQ_D6, SQ_E6, SQ_F6, SQ_G6, SQ_H6,
  SQ_A7, SQ_B7, SQ_C7, SQ_D7, SQ_E7, SQ_F7, SQ_G7, SQ_H7,
  SQ_A8, SQ_B8, SQ_C8, SQ_D8, SQ_E8, SQ_F8, SQ_G8, SQ_H8,
  SQ_NONE,

  SQUARE_NB = 64,

  DELTA_N =  8,
  DELTA_E =  1,
  DELTA_S = -8,
  DELTA_W = -1,

  DELTA_NN = DELTA_N + DELTA_N,
  DELTA_NE = DELTA_N + DELTA_E,
  DELTA_SE = DELTA_S + DELTA_E,
  DELTA_SS = DELTA_S + DELTA_S,
  DELTA_SW = DELTA_S + DELTA_W,
  DELTA_NW = DELTA_N + DELTA_W
};

enum File {
  FILE_A, FILE_B, FILE_C, FILE_D, FILE_E, FILE_F, FILE_G, FILE_H, FILE_NB
};

enum Rank {
  RANK_1, RANK_2, RANK_3, RANK_4, RANK_5, RANK_6, RANK_7, RANK_8, RANK_NB
};


/// Score enum stores a middlegame and an endgame value in a single integer
/// (enum). The least significant 16 bits are used to store the endgame value
/// and the upper 16 bits are used to store the middlegame value.
enum Score : int { SCORE_ZERO };

inline Score make_score(int mg, int eg) {
  return Score((mg << 16) + eg);
}

/// Extracting the signed lower and upper 16 bits is not so trivial because
/// according to the standard a simple cast to short is implementation defined
/// and so is a right shift of a signed integer.
inline Value mg_value(Score s) {

  union { uint16_t u; int16_t s; } mg = { uint16_t(unsigned(s + 0x8000) >> 16) };
  return Value(mg.s);
}

inline Value eg_value(Score s) {

  union { uint16_t u; int16_t s; } eg = { uint16_t(unsigned(s)) };
  return Value(eg.s);
}

#define ENABLE_BASE_OPERATORS_ON(T)                             \
inline T operator+(T d1, T d2) { return T(int(d1) + int(d2)); } \
inline T operator-(T d1, T d2) { return T(int(d1) - int(d2)); } \
inline T operator*(int i, T d) { return T(i * int(d)); }        \
inline T operator*(T d, int i) { return T(int(d) * i); }        \
inline T operator-(T d) { return T(-int(d)); }                  \
inline T& operator+=(T& d1, T d2) { return d1 = d1 + d2; }      \
inline T& operator-=(T& d1, T d2) { return d1 = d1 - d2; }      \
inline T& operator*=(T& d, int i) { return d = T(int(d) * i); }

#define ENABLE_FULL_OPERATORS_ON(T)                             \
ENABLE_BASE_OPERATORS_ON(T)                                     \
inline T& operator++(T& d) { return d = T(int(d) + 1); }        \
inline T& operator--(T& d) { return d = T(int(d) - 1); }        \
inline T operator/(T d, int i) { return T(int(d) / i); }        \
inline int operator/(T d1, T d2) { return int(d1) / int(d2); }  \
inline T& operator/=(T& d, int i) { return d = T(int(d) / i); }

ENABLE_FULL_OPERATORS_ON(Value)
ENABLE_FULL_OPERATORS_ON(PieceType)
ENABLE_FULL_OPERATORS_ON(Piece)
ENABLE_FULL_OPERATORS_ON(Color)
ENABLE_FULL_OPERATORS_ON(Depth)
ENABLE_FULL_OPERATORS_ON(Square)
ENABLE_FULL_OPERATORS_ON(File)
ENABLE_FULL_OPERATORS_ON(Rank)

ENABLE_BASE_OPERATORS_ON(Score)

#undef ENABLE_FULL_OPERATORS_ON
#undef ENABLE_BASE_OPERATORS_ON

/// Additional operators to add integers to a Value
inline Value operator+(Value v, int i) { return Value(int(v) + i); }
inline Value operator-(Value v, int i) { return Value(int(v) - i); }
inline Value& operator+=(Value& v, int i) { return v = v + i; }
inline Value& operator-=(Value& v, int i) { return v = v - i; }

/// Only declared but not defined. We don't want to multiply two scores due to
/// a very high risk of overflow. So user should explicitly convert to integer.
inline Score operator*(Score s1, Score s2);

/// Division of a Score must be handled separately for each term
inline Score operator/(Score s, int i) {
  return make_score(mg_value(s) / i, eg_value(s) / i);
}

extern Value PieceValue[PHASE_NB][PIECE_NB];

inline Color operator~(Color c) {
  return Color(c ^ BLACK);
}

inline Square operator~(Square s) {
  return Square(s ^ SQ_A8); // Vertical flip SQ_A1 -> SQ_A8
}

inline CastlingRight operator|(Color c, CastlingSide s) {
  return CastlingRight(WHITE_OO << ((s == QUEEN_SIDE) + 2 * c));
}

inline Value mate_in(int ply) {
  return VALUE_MATE - ply;
}

inline Value mated_in(int ply) {
  return -VALUE_MATE + ply;
}

inline Square make_square(File f, Rank r) {
  return Square((r << 3) | f);
}

inline Piece make_piece(Color c, PieceType pt) {
  return Piece((c << 3) | pt);
}

inline PieceType type_of(Piece pc) {
  return PieceType(pc & 7);
}

inline Color color_of(Piece pc) {
  assert(pc != NO_PIECE);
  return Color(pc >> 3);
}

inline bool is_ok(Square s) {
  return s >= SQ_A1 && s <= SQ_H8;
}

inline File file_of(Square s) {
  return File(s & 7);
}

inline Rank rank_of(Square s) {
  return Rank(s >> 3);
}

inline Square relative_square(Color c, Square s) {
  return Square(s ^ (c * 56));
}

inline Rank relative_rank(Color c, Rank r) {
  return Rank(r ^ (c * 7));
}

inline Rank relative_rank(Color c, Square s) {
  return relative_rank(c, rank_of(s));
}

inline bool opposite_colors(Square s1, Square s2) {
  int s = int(s1) ^ int(s2);
  return ((s >> 3) ^ s) & 1;
}

inline Square pawn_push(Color c) {
  return c == WHITE ? DELTA_N : DELTA_S;
}

inline Square from_sq(Move m) {
  return Square((m >> 6) & 0x3F);
}

inline Square to_sq(Move m) {
  return Square(m & 0x3F);
}

inline MoveType type_of(Move m) {
  return MoveType(m & (3 << 14));
}

inline PieceType promotion_type(Move m) {
  return PieceType(((m >> 12) & 3) + KNIGHT);
}

inline Move make_move(Square from, Square to) {
  return Move(to | (from << 6));
}

template<MoveType T>
inline Move make(Square from, Square to, PieceType pt = KNIGHT) {
  return Move(to | (from << 6) | T | ((pt - KNIGHT) << 12));
}

inline bool is_ok(Move m) {
  return from_sq(m) != to_sq(m); // Catch MOVE_NULL and MOVE_NONE
}

#else // #if !defined(CLOWNISH)

#if 1
	#define ASSERT_ERR(a) if (!(a)) { fprintf(stdout, "info string %s : %d : Assertion (%s) failed.\n", __FILE__, __LINE__, #a); ::exit(0); }
#else
	#define ASSERT_ERR(a) if (!(a)) { fprintf(stderr, "%s : %d : Assertion (%s) failed.\n", __FILE__, __LINE__, #a); ::exit(0); }
#endif

#if defined(NDEBUG)
	#define ASSERT_DBG(a)
#else
	#define ASSERT_DBG(a) ASSERT_ERR(a)
#endif

#if defined(_MSC_VER)
#define UNREACHABLE __assume(0)
#else
#define UNREACHABLE __builtin_unreachable()
#endif

#if defined(_MSC_VER)

inline int lsb_index(uint32_t mask) {
	unsigned long idx;
	_BitScanForward(&idx, mask);
	return static_cast<int>(idx);
}
inline int msb_index(uint32_t mask) {
	unsigned long idx;
	_BitScanReverse(&idx, mask);
	return static_cast<int>(idx);
}

inline int lsb_index(uint64_t mask) {
	unsigned long idx;
	_BitScanForward64(&idx, mask);
	return static_cast<int>(idx);
}
inline int msb_index(uint64_t mask) {
	unsigned long idx;
	_BitScanReverse64(&idx, mask);
	return static_cast<int>(idx);
}

#elif defined(__GNUC__)

inline int lsb_index(uint64_t u) {
	uint64_t idx;
	__asm__("bsfq %1, %0" : "=r" (idx) : "rm" (u));
	return static_cast<int>(idx);
}

inline int lsb_index(uint32_t u) {
	uint32_t idx;
	__asm__("bsf %1, %0" : "=r" (idx) : "rm" (u));
	return static_cast<int>(idx);
}

inline int msb_index(uint64_t u) {
	uint64_t idx;
	__asm__("bsrq %1, %0" : "=r" (idx) : "rm" (u));
	return static_cast<int>(idx);
}

inline int msb_index(uint32_t u) {
	uint32_t idx;
	__asm__("bsr %1, %0" : "=r" (idx) : "rm" (u));
	return static_cast<int>(idx);
}

#endif

template <class T> inline void lsb_clear(T *x) { *x &= *x - 1; }

union Union64 {

	uint64_t q;
	uint32_t d[2];
	uint16_t w[4];
	uint8_t  b[8];
};

typedef uint64_t Key;

const int MAX_MOVES = 640;
const int MAX_PLY   = 128;

enum Color {
	BLACK, WHITE, NO_COLOR, COLOR_NB = 2
};

enum ScaleFactor {
  SCALE_FACTOR_DRAW    = 0,
  SCALE_FACTOR_ONEPAWN = 48,
  SCALE_FACTOR_NORMAL  = 64,
  SCALE_FACTOR_MAX     = 128,
  SCALE_FACTOR_NONE    = 255
};

enum Bound {
	BOUND_NONE,
	BOUND_UPPER,
	BOUND_LOWER,
	BOUND_EXACT = BOUND_UPPER | BOUND_LOWER
};

enum Value : int {
	VALUE_ZERO       = 0,
	VALUE_DRAW       = 0,

	VALUE_SHEK_BOUND = 30000,
	VALUE_SHEK       = VALUE_SHEK_BOUND + MAX_PLY,
	VALUE_MATE_BOUND = VALUE_SHEK + 1,
	VALUE_MATE       = VALUE_MATE_BOUND + MAX_PLY,
	VALUE_INFINITE   = VALUE_MATE + 1,
	VALUE_NONE       = VALUE_INFINITE + 1,

	VALUE_KNOWN_WIN  = VALUE_SHEK_BOUND - 1,

	VALUE_MATE_IN_MAX_PLY  =  VALUE_MATE_BOUND,
	VALUE_MATED_IN_MAX_PLY = -VALUE_MATE_BOUND,

	FV_SCALE = 32,
};

enum Piece {
	EMPTY         = 0x00, WALL          = 0x10, PIECE_NB = 0x20, PROMOTED = 0x08,

	B_PAWN        = 0x01, W_PAWN        = 0x11,
	B_LANCE       = 0x02, W_LANCE       = 0x12,
	B_KNIGHT      = 0x03, W_KNIGHT      = 0x13,
	B_SILVER      = 0x04, W_SILVER      = 0x14,
	B_GOLD        = 0x05, W_GOLD        = 0x15,
	B_BISHOP      = 0x06, W_BISHOP      = 0x16,
	B_ROOK        = 0x07, W_ROOK        = 0x17,
	B_KING        = 0x08, W_KING        = 0x18,
	B_PROM_PAWN   = 0x09, W_PROM_PAWN   = 0x19,
	B_PROM_LANCE  = 0x0A, W_PROM_LANCE  = 0x1A,
	B_PROM_KNIGHT = 0x0B, W_PROM_KNIGHT = 0x1B,
	B_PROM_SILVER = 0x0C, W_PROM_SILVER = 0x1C,
	B_PROM_GOLD   = 0x0D, W_PROM_GOLD   = 0x1D,
	B_PROM_BISHOP = 0x0E, W_PROM_BISHOP = 0x1E,
	B_PROM_ROOK   = 0x0F, W_PROM_ROOK   = 0x1F,
};

// 多用している (1u << piece) をどう書くか
using PieceMask = uint32_t;

using Index = int;
enum : Index {
	INDEX_PAWN   = 0,
	INDEX_LANCE  = INDEX_PAWN   + 18,
	INDEX_KNIGHT = INDEX_LANCE  +  4,
	INDEX_SILVER = INDEX_KNIGHT +  4,
	INDEX_GOLD   = INDEX_SILVER +  4,
	INDEX_BISHOP = INDEX_GOLD   +  4,
	INDEX_ROOK   = INDEX_BISHOP +  2,
	INDEX_KING   = INDEX_ROOK   +  2,
	INDEX_NB     = INDEX_KING   +  2,
};

// 多用している (1ull << idx) をどう書くか
using IndexMask = uint64_t;
enum : IndexMask {
	INDEX_MASK_PAWN   = 0x3FFFFULL << INDEX_PAWN,
	INDEX_MASK_LANCE  =     0xFULL << INDEX_LANCE,
	INDEX_MASK_KNIGHT =     0xFULL << INDEX_KNIGHT,
	INDEX_MASK_SILVER =     0xFULL << INDEX_SILVER,
	INDEX_MASK_GOLD   =     0xFULL << INDEX_GOLD,
	INDEX_MASK_BISHOP =     0x3ULL << INDEX_BISHOP,
	INDEX_MASK_ROOK   =     0x3ULL << INDEX_ROOK,
	INDEX_MASK_KING   =     0x3ULL << INDEX_KING,
};

enum {
	// (MSB) ...PPPPP.LLL.NNN.SSS.GGG..BB..RR (LSB)
	HAND_SHIFT_PAWN    = 24, //  0;
	HAND_SHIFT_LANCE   = 20, //  6;
	HAND_SHIFT_KNIGHT  = 16, // 10;
	HAND_SHIFT_SILVER  = 12, // 14;
	HAND_SHIFT_GOLD    =  8, // 18;
	HAND_SHIFT_BISHOP  =  4, // 22;
	HAND_SHIFT_ROOK    =  0, // 25;

	HAND_MASK_PAWN   = 0x1F << HAND_SHIFT_PAWN,
	HAND_MASK_LANCE  = 0x07 << HAND_SHIFT_LANCE,
	HAND_MASK_KNIGHT = 0x07 << HAND_SHIFT_KNIGHT,
	HAND_MASK_SILVER = 0x07 << HAND_SHIFT_SILVER,
	HAND_MASK_GOLD   = 0x07 << HAND_SHIFT_GOLD,
	HAND_MASK_BISHOP = 0x03 << HAND_SHIFT_BISHOP,
	HAND_MASK_ROOK   = 0x03 << HAND_SHIFT_ROOK,
	HAND_MASK_BORROW = 0xE08888CC,
};

enum Depth {

	ONE_PLY = 1,

	DEPTH_ZERO          =  0,
	DEPTH_QS_CHECKS     =  0,
	DEPTH_QS_NO_CHECKS  = -1,
	DEPTH_QS_RECAPTURES = -5,

	DEPTH_NONE = -6,
	DEPTH_MAX  = MAX_PLY
};

enum File {
	FILE_1 = 1, FILE_2, FILE_3, FILE_4, FILE_5, FILE_6, FILE_7, FILE_8, FILE_9, FILE_NB,
};

enum Rank {
	RANK_A = 1, RANK_B, RANK_C, RANK_D, RANK_E, RANK_F, RANK_G, RANK_H, RANK_I, RANK_NB,
};

enum Square {
	// 相対座標
	INC_NONE =   0,
	INC_LUU  = -35,              INC_RUU = -33,
	INC_LU   = -18, INC_U = -17, INC_RU  = -16,
	INC_L    =  -1,              INC_R   =  +1,
	INC_LD   = +16, INC_D = +17, INC_RD  = +18,
	INC_LDD  = +33,              INC_RDD = +35,

	DELTA_OFFSET = 144,
	DELTA_NB     = DELTA_OFFSET * 2 + 1,

	// 絶対座標
	SQ_9A = INC_D * 1 + INC_D + 4, SQ_8A, SQ_7A, SQ_6A, SQ_5A, SQ_4A, SQ_3A, SQ_2A, SQ_1A,
	SQ_9B = INC_D * 2 + INC_D + 4, SQ_8B, SQ_7B, SQ_6B, SQ_5B, SQ_4B, SQ_3B, SQ_2B, SQ_1B,
	SQ_9C = INC_D * 3 + INC_D + 4, SQ_8C, SQ_7C, SQ_6C, SQ_5C, SQ_4C, SQ_3C, SQ_2C, SQ_1C,
	SQ_9D = INC_D * 4 + INC_D + 4, SQ_8D, SQ_7D, SQ_6D, SQ_5D, SQ_4D, SQ_3D, SQ_2D, SQ_1D,
	SQ_9E = INC_D * 5 + INC_D + 4, SQ_8E, SQ_7E, SQ_6E, SQ_5E, SQ_4E, SQ_3E, SQ_2E, SQ_1E,
	SQ_9F = INC_D * 6 + INC_D + 4, SQ_8F, SQ_7F, SQ_6F, SQ_5F, SQ_4F, SQ_3F, SQ_2F, SQ_1F,
	SQ_9G = INC_D * 7 + INC_D + 4, SQ_8G, SQ_7G, SQ_6G, SQ_5G, SQ_4G, SQ_3G, SQ_2G, SQ_1G,
	SQ_9H = INC_D * 8 + INC_D + 4, SQ_8H, SQ_7H, SQ_6H, SQ_5H, SQ_4H, SQ_3H, SQ_2H, SQ_1H,
	SQ_9I = INC_D * 9 + INC_D + 4, SQ_8I, SQ_7I, SQ_6I, SQ_5I, SQ_4I, SQ_3I, SQ_2I, SQ_1I,

	SQUARE_NONE = 0,
	SQUARE_DROP = 0,
	SQUARE_NB   = INC_D * 13,
};

// ........................tttttttt (bit  0- 7) : 移動先
// ................ffffffff........ (bit  8-15) : 移動元
// ...........ppppp................ (bit 16-20) : 移動元の駒
// ..........d..................... (bit 21)    : 駒打ちフラグ
// .........n...................... (bit 22)    : 駒成りフラグ
// ....ccccc....................... (bit 23-27) : 移動先の駒（相手の駒）

enum Move {
	MOVE_NONE = 0,
	MOVE_NULL = SQ_9A | SQ_9A << 8,
};


/// Score enum stores a middlegame and an endgame value in a single integer
/// (enum). The least significant 16 bits are used to store the endgame value
/// and the upper 16 bits are used to store the middlegame value.
enum Score : int { SCORE_ZERO };

inline Score make_score(int mg, int eg) {
  return Score((mg << 16) + eg);
}

/// Extracting the signed lower and upper 16 bits is not so trivial because
/// according to the standard a simple cast to short is implementation defined
/// and so is a right shift of a signed integer.
inline Value mg_value(Score s) {

  union { uint16_t u; int16_t s; } mg = { uint16_t(unsigned(s + 0x8000) >> 16) };
  return Value(mg.s);
}

inline Value eg_value(Score s) {

  union { uint16_t u; int16_t s; } eg = { uint16_t(unsigned(s)) };
  return Value(eg.s);
}

#define ENABLE_BASE_OPERATORS_ON(T)                             \
inline T operator+(T d1, T d2) { return T(int(d1) + int(d2)); } \
inline T operator-(T d1, T d2) { return T(int(d1) - int(d2)); } \
inline T operator*(int i, T d) { return T(i * int(d)); }        \
inline T operator*(T d, int i) { return T(int(d) * i); }        \
inline T operator-(T d) { return T(-int(d)); }                  \
inline T& operator+=(T& d1, T d2) { return d1 = d1 + d2; }      \
inline T& operator-=(T& d1, T d2) { return d1 = d1 - d2; }      \
inline T& operator*=(T& d, int i) { return d = T(int(d) * i); }

#define ENABLE_FULL_OPERATORS_ON(T)                             \
ENABLE_BASE_OPERATORS_ON(T)                                     \
inline T& operator++(T& d) { return d = T(int(d) + 1); }        \
inline T& operator--(T& d) { return d = T(int(d) - 1); }        \
inline T operator/(T d, int i) { return T(int(d) / i); }        \
inline int operator/(T d1, T d2) { return int(d1) / int(d2); }  \
inline T& operator/=(T& d, int i) { return d = T(int(d) / i); }

ENABLE_FULL_OPERATORS_ON(Value)
ENABLE_FULL_OPERATORS_ON(Piece)
ENABLE_FULL_OPERATORS_ON(Color)
ENABLE_FULL_OPERATORS_ON(Depth)
ENABLE_FULL_OPERATORS_ON(Square)
ENABLE_FULL_OPERATORS_ON(File)
ENABLE_FULL_OPERATORS_ON(Rank)

ENABLE_BASE_OPERATORS_ON(Score)

#undef ENABLE_FULL_OPERATORS_ON
#undef ENABLE_BASE_OPERATORS_ON

/// Additional operators to add integers to a Value
inline Value operator+(Value v, int i) { return Value(int(v) + i); }
inline Value operator-(Value v, int i) { return Value(int(v) - i); }
inline Value& operator+=(Value& v, int i) { return v = v + i; }
inline Value& operator-=(Value& v, int i) { return v = v - i; }

/// Only declared but not defined. We don't want to multiply two scores due to
/// a very high risk of overflow. So user should explicitly convert to integer.
inline Score operator*(Score s1, Score s2);

/// Division of a Score must be handled separately for each term
inline Score operator/(Score s, int i) {
  return make_score(mg_value(s) / i, eg_value(s) / i);
}

// evaluate.cpp
extern int ValuePieceMg[48];
extern int ValuePromotionMg[48];
extern int ValueCaptureMg[48];

// position.cpp
extern PieceMask DeltaStepPieces[DELTA_NB];
extern PieceMask DeltaSlidingPieces[DELTA_NB];
extern Square DeltaToInc[DELTA_NB];
extern const Square DirectionToInc[];
extern const Square PieceMoveInc[PIECE_NB][16];
extern File SquareToFile[SQUARE_NB];
extern Rank SquareToRank[SQUARE_NB];
extern int SquareToFileBit[SQUARE_NB];
extern int SquareToRankBit[SQUARE_NB];
extern Square SquareTo81[SQUARE_NB];
extern Square SquareToInv81[SQUARE_NB];
extern Square SquareFrom81[81];
extern PieceMask SquarePromotionPieces[SQUARE_NB];
extern PieceMask SquareUnpromotionPieces[SQUARE_NB];


///
///
///

// 手番
inline Color operator ~ (const Color c) { return Color(c ^ 1); }

// 駒
inline Piece PAWN(Color c)        { return c == BLACK ? B_PAWN        : W_PAWN       ; }
inline Piece LANCE(Color c)       { return c == BLACK ? B_LANCE       : W_LANCE      ; }
inline Piece KNIGHT(Color c)      { return c == BLACK ? B_KNIGHT      : W_KNIGHT     ; }
inline Piece SILVER(Color c)      { return c == BLACK ? B_SILVER      : W_SILVER     ; }
inline Piece GOLD(Color c)        { return c == BLACK ? B_GOLD        : W_GOLD       ; }
inline Piece BISHOP(Color c)      { return c == BLACK ? B_BISHOP      : W_BISHOP     ; }
inline Piece ROOK(Color c)        { return c == BLACK ? B_ROOK        : W_ROOK       ; }
inline Piece KING(Color c)        { return c == BLACK ? B_KING        : W_KING       ; }
inline Piece PROM_PAWN(Color c)   { return c == BLACK ? B_PROM_PAWN   : W_PROM_PAWN  ; }
inline Piece PROM_LANCE(Color c)  { return c == BLACK ? B_PROM_LANCE  : W_PROM_LANCE ; }
inline Piece PROM_KNIGHT(Color c) { return c == BLACK ? B_PROM_KNIGHT : W_PROM_KNIGHT; }
inline Piece PROM_SILVER(Color c) { return c == BLACK ? B_PROM_SILVER : W_PROM_SILVER; }
inline Piece PROM_BISHOP(Color c) { return c == BLACK ? B_PROM_BISHOP : W_PROM_BISHOP; }
inline Piece PROM_ROOK(Color c)   { return c == BLACK ? B_PROM_ROOK   : W_PROM_ROOK  ; }

inline bool PIECE_IS_PAWN(Piece p) { return (p & 15) == B_PAWN; }
inline bool PIECE_IS_KING(Piece p) { return (p & 15) == B_KING; }
// ※ 空きマスの可能性があるときは使えない
inline bool PIECE_IS_OPPONENT(Piece p, Color c) { return c == BLACK ? p > WALL : p < WALL; }

inline Color PIECE_TO_COLOR(Piece p) { return Color(p >> 4); }

// 持ち駒
inline int HAND_TO_PAWN(int hand)   { return (hand & HAND_MASK_PAWN)   >> HAND_SHIFT_PAWN  ; }
inline int HAND_TO_LANCE(int hand)  { return (hand & HAND_MASK_LANCE)  >> HAND_SHIFT_LANCE ; }
inline int HAND_TO_KNIGHT(int hand) { return (hand & HAND_MASK_KNIGHT) >> HAND_SHIFT_KNIGHT; }
inline int HAND_TO_SILVER(int hand) { return (hand & HAND_MASK_SILVER) >> HAND_SHIFT_SILVER; }
inline int HAND_TO_GOLD(int hand)   { return (hand & HAND_MASK_GOLD)   >> HAND_SHIFT_GOLD  ; }
inline int HAND_TO_BISHOP(int hand) { return (hand & HAND_MASK_BISHOP) >> HAND_SHIFT_BISHOP; }
inline int HAND_TO_ROOK(int hand)   { return (hand & HAND_MASK_ROOK)   >> HAND_SHIFT_ROOK  ; }

// 相対座標
inline PieceMask DELTA_STEP_PIECES(Square d)         { return DeltaStepPieces[DELTA_OFFSET + d]; }
inline PieceMask DELTA_SLIDING_PIECES(Square d)      { return DeltaSlidingPieces[DELTA_OFFSET + d]; }
inline Square    DELTA_TO_INC(Square d)              { return DeltaToInc[DELTA_OFFSET + d]; }

// 絶対座標 -将棋盤座標で指定-
inline Square SQ_MAKE(File f, Rank r)   { return Square((int)r * INC_D + INC_D + 13 - f); }
inline Square SQ81_MAKE(File f, Rank r) { return Square((int)r * 9 - f); }
inline Square SQ_INV(Square sq)         { return Square(SQUARE_NB - 1 - sq); }
inline Square SQ81_INV(Square sq81)     { return Square(80 - sq81); }

// 成/不成の判定
inline PieceMask CAN_PROMOTION(Square toSq, Square fromSq, Piece p) {
	return (1u << p) & (SquarePromotionPieces[toSq] | SquarePromotionPieces[fromSq]);
}
inline PieceMask CAN_UNPROMOTION(Square toSq, Piece p) {
	return (1u << p) & SquareUnpromotionPieces[toSq];
}

// 駒取りの判定…使われ方が微妙
inline bool CAN_CAPTURE(Piece p, Color c) {
	return static_cast<uint32_t>(p - (c == BLACK ? B_PAWN : WALL)) >= static_cast<uint32_t>(WALL);
}

// 指し手…クラス化するか否か
inline Move DROP_MAKE(Square toSq, Piece piece) { return Move(toSq | piece << 16 | 1 << 21); }
inline Move MOVE_MAKE(Square toSq, Square fromSq, Piece piece, Piece capture, int promotion) { return Move(toSq | fromSq << 8 | piece << 16 | capture << 23 | promotion << 22); }
inline Square MOVE_TOSQ(Move m)    { return Square(m & 0xFF); }
inline Square MOVE_FROMSQ(Move m)  { return Square((m >> 8) & 0xFF); }
inline Piece MOVE_PIECE(Move m)    { return Piece((m >> 16) & 0x1F); }
inline Piece MOVE_CAPTURED(Move m) { return Piece((m >> 23) & 0x1F); }
inline Piece MOVED_PIECE(Move m)   { return Piece(((m >> 16) & 0x1F) | (m & (1 << 22)) >> 19); }
inline bool MOVE_IS_PROMOTION(Move m) { return (m & (1<<22)) != 0; }
inline bool MOVE_IS_DROP(Move m)      { return (m & (1<<21)) != 0; }
inline bool MOVE_IS_DROP_PAWN(Move m) { return (m & 0xFFF00) == (B_PAWN << 16); }
inline bool MOVE_IS_QUIET(Move m)     { return (m & (0x3F<<22)) == 0; }


inline Value mate_in(int ply) {
  return VALUE_MATE - ply;
}

inline Value mated_in(int ply) {
  return -(VALUE_MATE - ply + 1);
}

inline bool is_ok(Move m) {
  return MOVE_FROMSQ(m) != MOVE_TOSQ(m);
}

#endif

#endif // #ifndef TYPES_H_INCLUDED
