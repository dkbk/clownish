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

#include "mate.h"
#include "movegen.h"
#include "position.h"


// TODO : 先手後手ともに相手玉の上部から判定する、というのはさすがに面倒か？

// ８近傍のビット割り当て ※ 先手後手共通
// ・ ・ ・ ・ ・
// ・ ０ １ ２ ・
// ・ ３ 玉 ４ ・
// ・ ５ ６ ７ ・
// ・ ・ ・ ・ ・

// 各マス別、各駒別の８近傍の利きマスク ※ 移動先に適用、移動先に利きがあることが前提
// uint8_t IncrementMask[8][32] に実装し直すか…？
static const Union64 IncrementMask[PIECE_NB] = {

	~0x2400810000810024ull, // ※ 龍の斜め王手マスクを消すためのマスク
	0x8040201008040201, // 歩
	0x8042201008040201, // 香
	0,
	0x9058281008040201, // 銀
	0xD0F868962B040701, // 金
	0x8140241008240281, // 角
	0x80E2209C39044701, // 飛
	0,
	0xD0F868962B040701, // と
	0xD0F868962B040701, // 杏
	0xD0F868962B040701, // 圭
	0xD0F868962B040701, // 全
	0,
	0xD1F86CD66B361F8B, // 馬
	0xD0FA68DE7B165F0B | 0x2400810000810024, // 龍
	0,
	0x8040201008040201, // 歩
	0x8040201008044201, // 香
	0,
	0x8040201008141A09, // 銀
	0x80E020D469161F0B, // 金
	0x8140241008240281, // 角
	0x80E2209C39044701, // 飛
	0,
	0x80E020D469161F0B, // と
	0x80E020D469161F0B, // 杏
	0x80E020D469161F0B, // 圭
	0x80E020D469161F0B, // 全
	0,
	0xD1F86CD66B361F8B, // 馬
	0xD0FA68DE7B165F0B | 0x2400810000810024, // 龍
};

// 利きが減る可能性がある８近傍のマスク ※ 移動先に適用
static const uint8_t DecrementMask[12] = {
	0x2E, 0x1D, 0x93, 0x63, 0xC6, 0xC9, 0xB8, 0x74, 0xDC, 0x79, 0x9E, 0x3B,
};

// ８近傍から「成って」王手できる駒の種類フラグ
static const PieceMask PromotionCheckMask[8] = {
	0x00DE00C0, 0x00DE00DE, 0x00DE00C0, 0x00DE00DE, 0x00DE00DE, 0x00C000DE, 0x00DE00DE, 0x00C000DE,
};

// ８近傍から「成らずに」王手できる駒の種類フラグ
static const PieceMask UnpromotionCheckMask[8] = {
	0xDE70C050, 0xDEB6DEA0, 0xDE70C050, 0xDEA0DEA0, 0xDEA0DEA0, 0xC050DE70, 0xDEA0DEB6, 0xC050DE70,
};

// 利きが増える可能性がある８近傍のマスク ※ 移動元に適用
static uint8_t IncrementDeltaMask[DELTA_NB] = {};

// 「玉移動候補位置」と「王手候補位置」のビットパターンから詰む可能性がある「駒打ち王手の駒種と位置」のビットリスト
static uint64_t DropMateTable[COLOR_NB][0x10000] = {};


/// 初期化
void mate_init() {

	// 駒種別の８近傍の王手可能マスク ※ 移動先に適用
	const uint8_t DropCheckMask[PIECE_NB] = {
		0, 0, 0x40, 0, 0xE5, 0xFA, 0xA5, 0x5A, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0x02, 0, 0xA7, 0x5F, 0xA5, 0x5A, 0, 0, 0, 0, 0, 0, 0, 0,
	};

	// 玉８近傍の「玉移動候補位置」と「王手候補位置」の組み合わせから
	// 駒打ちで詰む可能性がある「打つ駒種」と「打つ位置」を求めておく
	for (Piece piece = B_LANCE; piece <= W_ROOK; ++piece) {
		for (uint32_t bits = DropCheckMask[piece]; bits; lsb_clear(&bits)) {
			int dir = lsb_index(bits);

			for (uint32_t escapes = 0; escapes < 256; ++escapes) {
				if (escapes & ~IncrementMask[piece].b[dir])
					continue;

				for (uint32_t checks = 0; checks < 256; ++checks)
					if ((1 << dir) & checks)
						DropMateTable[piece >> 4][escapes * 256 + checks] |= 1ull << ((piece & 7) * 8 + dir);
			}
		}
	}

	// 移動元に利いている長い利きの延長支援（隣接王手１手詰めでは距離４までで足りる、はず）
	for (int dir0 = 0; dir0 < 8; ++dir0)
		for (int dir1 = 0; dir1 < 8; ++dir1)
			for (int i = 4; i; --i)
				IncrementDeltaMask[DELTA_OFFSET - DirectionToInc[dir0] - DirectionToInc[dir1] * i] |= 1 << dir0;
}

/// 受け方の駒取り判定
template <Color C>
static bool mate1_def_capture(const Position& pos, Square defKingSq, Square atkFromSq, Square atkToSq, IndexMask toDefs) {

	do {
		Square defFromSq = pos.square_at(lsb_index(toDefs));
		Square defLineInc = DELTA_TO_INC(defKingSq - defFromSq);
		if (defLineInc == INC_NONE)
			return true;

		if (atkFromSq != SQUARE_DROP && defLineInc == DELTA_TO_INC(defKingSq - atkFromSq)) {

			// 結果的（＝２手先）に defSq と fromSq の両駒が
			// 受け方の玉のライン上から外れるのでその状態が王手か否か
			Square sq = defKingSq;
			do sq = pos.skip_empties(sq, -defLineInc); while (sq == defFromSq || sq == atkFromSq);

			if (   !PIECE_IS_OPPONENT(pos.piece_on(sq), ~C)
			    || !((1u << pos.piece_on(sq)) & DELTA_SLIDING_PIECES(defLineInc)))
				return true;
		}
		else
			if (!pos.is_pinned(defKingSq, defFromSq, atkToSq, C))
				return true;

	} while (lsb_clear(&toDefs), toDefs);

	return false;
}

/// 駒打ちでの詰み判定
template <Color C>
static bool mate1_atk_drop(const Position& pos, uint32_t defKingEscapes, Square defKingSq, Square checkToSq) {

	for ( ; defKingEscapes; lsb_clear(&defKingEscapes)) {

		Square defKingToSq = defKingSq + DirectionToInc[lsb_index(defKingEscapes)];

		// 利きが残っているマスへの自爆
		IndexMask atks = pos.effects_on(defKingToSq, C);
		ASSERT_DBG(atks != 0ull);
		if (   (atks & (atks - 1))
		    || !(atks & pos.sliding_effects_on(checkToSq))
		    || DELTA_TO_INC(defKingToSq - checkToSq) != DELTA_TO_INC(checkToSq - pos.square_at(lsb_index(atks))))
			continue;

		return false;
	}

	return true;
}

/// 駒移動での詰み判定
template <Color C>
static bool mate1_atk_move(const Position& pos, uint32_t defKingEscapes, Square defKingSq, Square checkFromSq, Square checkToSq, Piece checker) {

	for ( ; defKingEscapes; lsb_clear(&defKingEscapes)) {

		Square defKingToSq = defKingSq + DirectionToInc[lsb_index(defKingEscapes)];

		// 利きが残っているマスへの自爆
		IndexMask atks = pos.effects_on(defKingToSq, C) & ~(1ull << pos.index_on(checkFromSq));
		if (atks && (   (atks & (atks - 1))
		             || !(atks & pos.sliding_effects_on(checkToSq))
		             || DELTA_TO_INC(defKingToSq - checkToSq) != DELTA_TO_INC(checkToSq - pos.square_at(lsb_index(atks)))))
			continue;

		// 龍の斜め王手への自爆
		if (   checker == PROM_ROOK(C)
		    && ((defKingToSq + checkToSq) & 1) == 0
		    && pos.piece_on((defKingToSq + checkToSq) / 2) == EMPTY)
			continue;

		// 利きが新たに通るマスへの自爆
		Square inc = DELTA_TO_INC(defKingToSq - checkFromSq);
		if (   inc != INC_NONE
		    && pos.is_pinned((inc == defKingToSq - defKingSq) ? defKingSq : defKingToSq, checkFromSq, checkToSq, C))
			continue;

		return false;
	}

	// 王手駒のピン判定は最後
	return !pos.is_pinned(pos.king_square(C), checkFromSq, checkToSq, ~C);
}

/// 隣接王手１手詰め判定
template <Color C>
Move mate_1ply(const Position& pos) {

	ASSERT_DBG(!pos.is_in_check());

	Square defKingSq = pos.king_square(~C);

	// 受け方玉の８近傍の情報収集
	// ※ この段階では"攻め方の駒の移動可否"や"攻め方の利きの数"は調べない

	// 攻め方の利きマスク
	uint32_t candidateChecks = 0;
	if (pos.effects_on(defKingSq + INC_LU, C)) candidateChecks |= 1 << 0;
	if (pos.effects_on(defKingSq + INC_U , C)) candidateChecks |= 1 << 1;
	if (pos.effects_on(defKingSq + INC_RU, C)) candidateChecks |= 1 << 2;
	if (pos.effects_on(defKingSq + INC_L , C)) candidateChecks |= 1 << 3;
	if (pos.effects_on(defKingSq + INC_R , C)) candidateChecks |= 1 << 4;
	if (pos.effects_on(defKingSq + INC_LD, C)) candidateChecks |= 1 << 5;
	if (pos.effects_on(defKingSq + INC_D , C)) candidateChecks |= 1 << 6;
	if (pos.effects_on(defKingSq + INC_RD, C)) candidateChecks |= 1 << 7;

	if (!candidateChecks)
		return MOVE_NONE; // 桂馬王手で詰む可能性はゼロではないけど…

	// 攻め方の利きを反映しない玉移動マスク
	uint32_t candidateEscapes = 0;
	if (CAN_CAPTURE(pos.piece_on(defKingSq + INC_LU), ~C)) candidateEscapes |= 1 << 0;
	if (CAN_CAPTURE(pos.piece_on(defKingSq + INC_U ), ~C)) candidateEscapes |= 1 << 1;
	if (CAN_CAPTURE(pos.piece_on(defKingSq + INC_RU), ~C)) candidateEscapes |= 1 << 2;
	if (CAN_CAPTURE(pos.piece_on(defKingSq + INC_L ), ~C)) candidateEscapes |= 1 << 3;
	if (CAN_CAPTURE(pos.piece_on(defKingSq + INC_R ), ~C)) candidateEscapes |= 1 << 4;
	if (CAN_CAPTURE(pos.piece_on(defKingSq + INC_LD), ~C)) candidateEscapes |= 1 << 5;
	if (CAN_CAPTURE(pos.piece_on(defKingSq + INC_D ), ~C)) candidateEscapes |= 1 << 6;
	if (CAN_CAPTURE(pos.piece_on(defKingSq + INC_RD), ~C)) candidateEscapes |= 1 << 7;

	// 攻め方の利きを反映した玉移動マスク
	uint32_t currentEscapes = candidateEscapes & ~candidateChecks;

	// ８近傍への駒打ち王手
	int hand = pos.pieces_in_hand(C);
	if (hand & (HAND_MASK_ROOK | HAND_MASK_BISHOP | HAND_MASK_GOLD | HAND_MASK_SILVER | HAND_MASK_LANCE)) {

		uint64_t drops = (hand & HAND_MASK_ROOK)  ? 0xFF00000000000000ull
		               : (hand & HAND_MASK_LANCE) ? 0x0000000000FF0000ull
		               : 0ULL;
		if (hand & HAND_MASK_BISHOP) drops |= 0x00FF000000000000ull;
		if (hand & HAND_MASK_GOLD)   drops |= 0x0000FF0000000000ull;
		if (hand & HAND_MASK_SILVER) drops |= 0x000000FF00000000ull;

		// ８近傍の状況から「詰む可能性がある（＝８近傍への長い利きが遮断されなければ詰む）」駒種と位置を取得
		drops &= DropMateTable[C][currentEscapes * 256 + candidateChecks];
		while (drops) {

			int toDir = lsb_index(drops) & 7; // 王手位置

			Square toSq = defKingSq + DirectionToInc[toDir];
			if (pos.piece_on(toSq) != EMPTY) {
				drops &= 0xFEFEFEFEFEFEFEFEull << toDir;
				continue;
			}

			IndexMask toDefs = pos.effects_on(toSq, ~C) & ~INDEX_MASK_KING;
			if (toDefs && mate1_def_capture<C>(pos, defKingSq, SQUARE_DROP, toSq, toDefs)) {
				drops &= 0xFEFEFEFEFEFEFEFEull << toDir;
				continue;
			}

			Piece piece = Piece(lsb_index(drops) / 8 + C * 16); // 王手駒

			if (!pos.sliding_effects_on(toSq, C))
				return DROP_MAKE(toSq, piece);

			if (mate1_atk_drop<C>(pos, candidateEscapes & DecrementMask[toDir] & ~IncrementMask[piece].b[toDir], defKingSq, toSq))
				return DROP_MAKE(toSq, piece);

			lsb_clear(&drops);
		}
	}

	const int KNIGHT_DIR_OFFSET = 8 + C * 2; // ※ 方向注意

	// 桂馬打ち王手
	if (!currentEscapes && (pos.pieces_in_hand(C) & HAND_MASK_KNIGHT)) {

		for (int i = 0; i < 2; ++i) {

			Square toSq = defKingSq - DirectionToInc[KNIGHT_DIR_OFFSET + i];
			if (pos.piece_on(toSq) != EMPTY)
				continue;

			IndexMask toDefs = pos.effects_on(toSq, ~C);
			if (toDefs && mate1_def_capture<C>(pos, defKingSq, SQUARE_DROP, toSq, toDefs))
				continue;

			if (!pos.sliding_effects_on(toSq, C))
				return DROP_MAKE(toSq, KNIGHT(C));

			if (mate1_atk_drop<C>(pos, candidateEscapes & DecrementMask[KNIGHT_DIR_OFFSET + i], defKingSq, toSq))
				return DROP_MAKE(toSq, KNIGHT(C));
		}
	}

	// 桂馬移動王手
	if (!(currentEscapes & (C == BLACK ? 0x1D : 0xB8))) {

		for (int i = 0; i < 2; ++i) {

			Square toSq = defKingSq - DirectionToInc[KNIGHT_DIR_OFFSET + i];
			if (!CAN_CAPTURE(pos.piece_on(toSq), C))
				continue;

			for (int j = 0; j < 2; ++j) {

				Square fromSq = toSq - DirectionToInc[KNIGHT_DIR_OFFSET + j];
				if (pos.piece_on(fromSq) != KNIGHT(C))
					continue;

				// 開き王手（＝両王手）か否か
				if (!pos.is_pinned(defKingSq, fromSq, toSq, C)) {
					IndexMask toDefs = pos.effects_on(toSq, ~C);
					if (toDefs && mate1_def_capture<C>(pos, defKingSq, fromSq, toSq, toDefs))
						continue;
				}

				if (mate1_atk_move<C>(pos, candidateEscapes, defKingSq, fromSq, toSq, KNIGHT(C)))
					return MOVE_MAKE(toSq, fromSq, KNIGHT(C), pos.piece_on(toSq), 0);
			}
		}
	}

	// ８近傍への駒移動王手
	for (uint32_t toBits = candidateChecks; toBits; lsb_clear(&toBits)) {

		int toDir = lsb_index(toBits);

		Square toSq = defKingSq + DirectionToInc[toDir];
		if (!CAN_CAPTURE(pos.piece_on(toSq), C))
			continue;

		IndexMask toDefs = pos.effects_on(toSq, ~C) & ~INDEX_MASK_KING;
		IndexMask toAtks = pos.effects_on(toSq, C); // 攻め方の玉は紐の可能性あり

		// （カウントしていない）真後ろからの味方の利き
		auto hidden_atk_sliders = [&] {
			Square fromSq = pos.square_at(lsb_index(toAtks));
			IndexMask fromAtks = pos.sliding_effects_on(fromSq, C);
			for ( ; fromAtks; lsb_clear(&fromAtks)) {
				Square sq = pos.square_at(lsb_index(fromAtks));
				if (DELTA_TO_INC(toSq - fromSq) == DELTA_TO_INC(fromSq - sq))
					return true;
			}
			return false;
		};

		if (   !(toAtks & (toAtks - 1))
		    && !hidden_atk_sliders())
			continue;

		IndexMask toChecks = toAtks & ~INDEX_MASK_KING;
		while (toChecks) {

			Index fromIdx = msb_index(toChecks); // 降順
			Square fromSq = pos.square_at(fromIdx);
			toChecks ^= 1ull << fromIdx;

			// 開き王手（＝両王手）か否か
			if (!pos.is_pinned(defKingSq, fromSq, toSq, C)) {

				// （カウントしていない）真後ろからの相手の利き
				auto hidden_def_sliders = [&] {
					IndexMask fromDefs = pos.sliding_effects_on(fromSq, ~C);
					for ( ; fromDefs; lsb_clear(&fromDefs)) {
						Square sq = pos.square_at(lsb_index(fromDefs));
						if (DELTA_TO_INC(toSq - fromSq) == DELTA_TO_INC(fromSq - sq))
							return !pos.is_pinned(defKingSq, sq, fromSq, C);
					}
					return false;
				};

				if (   (toDefs && mate1_def_capture<C>(pos, defKingSq, fromSq, toSq, toDefs))
				    || hidden_def_sliders())
					continue;
			}

			Piece piece = pos.piece_on(fromSq);

			// 成り王手
			if (   ((1u << piece) & PromotionCheckMask[toDir])
			    && CAN_PROMOTION(toSq, fromSq, piece)
			    && !(currentEscapes & ~(IncrementDeltaMask[DELTA_OFFSET + defKingSq - fromSq] | IncrementMask[piece + PROMOTED].b[toDir]))
			    && mate1_atk_move<C>(pos, candidateEscapes & ~(IncrementMask[piece + PROMOTED].b[toDir] & IncrementMask[0].b[toDir]), defKingSq, fromSq, toSq, piece + PROMOTED))
				return MOVE_MAKE(toSq, fromSq, piece, pos.piece_on(toSq), 1);

			// 不成王手
			if (   ((1u << piece) & UnpromotionCheckMask[toDir])
			    && CAN_UNPROMOTION(toSq, piece)
			    && !(currentEscapes & ~(IncrementDeltaMask[DELTA_OFFSET + defKingSq - fromSq] | IncrementMask[piece].b[toDir]))
			    && mate1_atk_move<C>(pos, candidateEscapes & ~(IncrementMask[piece].b[toDir] & IncrementMask[0].b[toDir]), defKingSq, fromSq, toSq, piece))
				return MOVE_MAKE(toSq, fromSq, piece, pos.piece_on(toSq), 0);
		}
	}

	return MOVE_NONE;
}

Move mate_1ply(const Position& pos) {

	return pos.black_to_move() ? mate_1ply<BLACK>(pos)
	                           : mate_1ply<WHITE>(pos);
}
