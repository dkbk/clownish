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

#include <cassert>

#include "movegen.h"
#include "position.h"

#if !defined(CLOWNISH)

namespace {

  template<CastlingRight Cr, bool Checks, bool Chess960>
  ExtMove* generate_castling(const Position& pos, ExtMove* moveList, Color us, const CheckInfo* ci) {

    static const bool KingSide = (Cr == WHITE_OO || Cr == BLACK_OO);

    if (pos.castling_impeded(Cr) || !pos.can_castle(Cr))
        return moveList;

    // After castling, the rook and king final positions are the same in Chess960
    // as they would be in standard chess.
    Square kfrom = pos.square<KING>(us);
    Square rfrom = pos.castling_rook_square(Cr);
    Square kto = relative_square(us, KingSide ? SQ_G1 : SQ_C1);
    Bitboard enemies = pos.pieces(~us);

    assert(!pos.checkers());

    const Square K = Chess960 ? kto > kfrom ? DELTA_W : DELTA_E
                              : KingSide    ? DELTA_W : DELTA_E;

    for (Square s = kto; s != kfrom; s += K)
        if (pos.attackers_to(s) & enemies)
            return moveList;

    // Because we generate only legal castling moves we need to verify that
    // when moving the castling rook we do not discover some hidden checker.
    // For instance an enemy queen in SQ_A1 when castling rook is in SQ_B1.
    if (Chess960 && (attacks_bb<ROOK>(kto, pos.pieces() ^ rfrom) & pos.pieces(~us, ROOK, QUEEN)))
        return moveList;

    Move m = make<CASTLING>(kfrom, rfrom);

    if (Checks && !pos.gives_check(m, *ci))
        return moveList;
    else
        (void)ci; // Silence a warning under MSVC

    *moveList++ = m;
    return moveList;
  }


  template<GenType Type, Square Delta>
  ExtMove* make_promotions(ExtMove* moveList, Square to, const CheckInfo* ci) {

    if (Type == CAPTURES || Type == EVASIONS || Type == NON_EVASIONS)
        *moveList++ = make<PROMOTION>(to - Delta, to, QUEEN);

    if (Type == QUIETS || Type == EVASIONS || Type == NON_EVASIONS)
    {
        *moveList++ = make<PROMOTION>(to - Delta, to, ROOK);
        *moveList++ = make<PROMOTION>(to - Delta, to, BISHOP);
        *moveList++ = make<PROMOTION>(to - Delta, to, KNIGHT);
    }

    // Knight promotion is the only promotion that can give a direct check
    // that's not already included in the queen promotion.
    if (Type == QUIET_CHECKS && (StepAttacksBB[W_KNIGHT][to] & ci->ksq))
        *moveList++ = make<PROMOTION>(to - Delta, to, KNIGHT);
    else
        (void)ci; // Silence a warning under MSVC

    return moveList;
  }


  template<Color Us, GenType Type>
  ExtMove* generate_pawn_moves(const Position& pos, ExtMove* moveList,
                               Bitboard target, const CheckInfo* ci) {

    // Compute our parametrized parameters at compile time, named according to
    // the point of view of white side.
    const Color    Them     = (Us == WHITE ? BLACK    : WHITE);
    const Bitboard TRank8BB = (Us == WHITE ? Rank8BB  : Rank1BB);
    const Bitboard TRank7BB = (Us == WHITE ? Rank7BB  : Rank2BB);
    const Bitboard TRank3BB = (Us == WHITE ? Rank3BB  : Rank6BB);
    const Square   Up       = (Us == WHITE ? DELTA_N  : DELTA_S);
    const Square   Right    = (Us == WHITE ? DELTA_NE : DELTA_SW);
    const Square   Left     = (Us == WHITE ? DELTA_NW : DELTA_SE);

    Bitboard emptySquares;

    Bitboard pawnsOn7    = pos.pieces(Us, PAWN) &  TRank7BB;
    Bitboard pawnsNotOn7 = pos.pieces(Us, PAWN) & ~TRank7BB;

    Bitboard enemies = (Type == EVASIONS ? pos.pieces(Them) & target:
                        Type == CAPTURES ? target : pos.pieces(Them));

    // Single and double pawn pushes, no promotions
    if (Type != CAPTURES)
    {
        emptySquares = (Type == QUIETS || Type == QUIET_CHECKS ? target : ~pos.pieces());

        Bitboard b1 = shift_bb<Up>(pawnsNotOn7)   & emptySquares;
        Bitboard b2 = shift_bb<Up>(b1 & TRank3BB) & emptySquares;

        if (Type == EVASIONS) // Consider only blocking squares
        {
            b1 &= target;
            b2 &= target;
        }

        if (Type == QUIET_CHECKS)
        {
            b1 &= pos.attacks_from<PAWN>(ci->ksq, Them);
            b2 &= pos.attacks_from<PAWN>(ci->ksq, Them);

            // Add pawn pushes which give discovered check. This is possible only
            // if the pawn is not on the same file as the enemy king, because we
            // don't generate captures. Note that a possible discovery check
            // promotion has been already generated amongst the captures.
            if (pawnsNotOn7 & ci->dcCandidates)
            {
                Bitboard dc1 = shift_bb<Up>(pawnsNotOn7 & ci->dcCandidates) & emptySquares & ~file_bb(ci->ksq);
                Bitboard dc2 = shift_bb<Up>(dc1 & TRank3BB) & emptySquares;

                b1 |= dc1;
                b2 |= dc2;
            }
        }

        while (b1)
        {
            Square to = pop_lsb(&b1);
            *moveList++ = make_move(to - Up, to);
        }

        while (b2)
        {
            Square to = pop_lsb(&b2);
            *moveList++ = make_move(to - Up - Up, to);
        }
    }

    // Promotions and underpromotions
    if (pawnsOn7 && (Type != EVASIONS || (target & TRank8BB)))
    {
        if (Type == CAPTURES)
            emptySquares = ~pos.pieces();

        if (Type == EVASIONS)
            emptySquares &= target;

        Bitboard b1 = shift_bb<Right>(pawnsOn7) & enemies;
        Bitboard b2 = shift_bb<Left >(pawnsOn7) & enemies;
        Bitboard b3 = shift_bb<Up   >(pawnsOn7) & emptySquares;

        while (b1)
            moveList = make_promotions<Type, Right>(moveList, pop_lsb(&b1), ci);

        while (b2)
            moveList = make_promotions<Type, Left >(moveList, pop_lsb(&b2), ci);

        while (b3)
            moveList = make_promotions<Type, Up   >(moveList, pop_lsb(&b3), ci);
    }

    // Standard and en-passant captures
    if (Type == CAPTURES || Type == EVASIONS || Type == NON_EVASIONS)
    {
        Bitboard b1 = shift_bb<Right>(pawnsNotOn7) & enemies;
        Bitboard b2 = shift_bb<Left >(pawnsNotOn7) & enemies;

        while (b1)
        {
            Square to = pop_lsb(&b1);
            *moveList++ = make_move(to - Right, to);
        }

        while (b2)
        {
            Square to = pop_lsb(&b2);
            *moveList++ = make_move(to - Left, to);
        }

        if (pos.ep_square() != SQ_NONE)
        {
            assert(rank_of(pos.ep_square()) == relative_rank(Us, RANK_6));

            // An en passant capture can be an evasion only if the checking piece
            // is the double pushed pawn and so is in the target. Otherwise this
            // is a discovery check and we are forced to do otherwise.
            if (Type == EVASIONS && !(target & (pos.ep_square() - Up)))
                return moveList;

            b1 = pawnsNotOn7 & pos.attacks_from<PAWN>(pos.ep_square(), Them);

            assert(b1);

            while (b1)
                *moveList++ = make<ENPASSANT>(pop_lsb(&b1), pos.ep_square());
        }
    }

    return moveList;
  }


  template<PieceType Pt, bool Checks>
  ExtMove* generate_moves(const Position& pos, ExtMove* moveList, Color us,
                          Bitboard target, const CheckInfo* ci) {

    assert(Pt != KING && Pt != PAWN);

    const Square* pl = pos.squares<Pt>(us);

    for (Square from = *pl; from != SQ_NONE; from = *++pl)
    {
        if (Checks)
        {
            if (    (Pt == BISHOP || Pt == ROOK || Pt == QUEEN)
                && !(PseudoAttacks[Pt][from] & target & ci->checkSquares[Pt]))
                continue;

            if (ci->dcCandidates && (ci->dcCandidates & from))
                continue;
        }

        Bitboard b = pos.attacks_from<Pt>(from) & target;

        if (Checks)
            b &= ci->checkSquares[Pt];

        while (b)
            *moveList++ = make_move(from, pop_lsb(&b));
    }

    return moveList;
  }


  template<Color Us, GenType Type>
  ExtMove* generate_all(const Position& pos, ExtMove* moveList, Bitboard target,
                        const CheckInfo* ci = nullptr) {

    const bool Checks = Type == QUIET_CHECKS;

    moveList = generate_pawn_moves<Us, Type>(pos, moveList, target, ci);
    moveList = generate_moves<KNIGHT, Checks>(pos, moveList, Us, target, ci);
    moveList = generate_moves<BISHOP, Checks>(pos, moveList, Us, target, ci);
    moveList = generate_moves<  ROOK, Checks>(pos, moveList, Us, target, ci);
    moveList = generate_moves< QUEEN, Checks>(pos, moveList, Us, target, ci);

    if (Type != QUIET_CHECKS && Type != EVASIONS)
    {
        Square ksq = pos.square<KING>(Us);
        Bitboard b = pos.attacks_from<KING>(ksq) & target;
        while (b)
            *moveList++ = make_move(ksq, pop_lsb(&b));
    }

    if (Type != CAPTURES && Type != EVASIONS && pos.can_castle(Us))
    {
        if (pos.is_chess960())
        {
            moveList = generate_castling<MakeCastling<Us,  KING_SIDE>::right, Checks, true>(pos, moveList, Us, ci);
            moveList = generate_castling<MakeCastling<Us, QUEEN_SIDE>::right, Checks, true>(pos, moveList, Us, ci);
        }
        else
        {
            moveList = generate_castling<MakeCastling<Us,  KING_SIDE>::right, Checks, false>(pos, moveList, Us, ci);
            moveList = generate_castling<MakeCastling<Us, QUEEN_SIDE>::right, Checks, false>(pos, moveList, Us, ci);
        }
    }

    return moveList;
  }

} // namespace


/// generate<CAPTURES> generates all pseudo-legal captures and queen
/// promotions. Returns a pointer to the end of the move list.
///
/// generate<QUIETS> generates all pseudo-legal non-captures and
/// underpromotions. Returns a pointer to the end of the move list.
///
/// generate<NON_EVASIONS> generates all pseudo-legal captures and
/// non-captures. Returns a pointer to the end of the move list.

template<GenType Type>
ExtMove* generate(const Position& pos, ExtMove* moveList) {

  assert(Type == CAPTURES || Type == QUIETS || Type == NON_EVASIONS);
  assert(!pos.checkers());

  Color us = pos.side_to_move();

  Bitboard target =  Type == CAPTURES     ?  pos.pieces(~us)
                   : Type == QUIETS       ? ~pos.pieces()
                   : Type == NON_EVASIONS ? ~pos.pieces(us) : 0;

  return us == WHITE ? generate_all<WHITE, Type>(pos, moveList, target)
                     : generate_all<BLACK, Type>(pos, moveList, target);
}

// Explicit template instantiations
template ExtMove* generate<CAPTURES>(const Position&, ExtMove*);
template ExtMove* generate<QUIETS>(const Position&, ExtMove*);
template ExtMove* generate<NON_EVASIONS>(const Position&, ExtMove*);


/// generate<QUIET_CHECKS> generates all pseudo-legal non-captures and knight
/// underpromotions that give check. Returns a pointer to the end of the move list.
template<>
ExtMove* generate<QUIET_CHECKS>(const Position& pos, ExtMove* moveList) {

  assert(!pos.checkers());

  Color us = pos.side_to_move();
  CheckInfo ci(pos);
  Bitboard dc = ci.dcCandidates;

  while (dc)
  {
     Square from = pop_lsb(&dc);
     PieceType pt = type_of(pos.piece_on(from));

     if (pt == PAWN)
         continue; // Will be generated together with direct checks

     Bitboard b = pos.attacks_from(Piece(pt), from) & ~pos.pieces();

     if (pt == KING)
         b &= ~PseudoAttacks[QUEEN][ci.ksq];

     while (b)
         *moveList++ = make_move(from, pop_lsb(&b));
  }

  return us == WHITE ? generate_all<WHITE, QUIET_CHECKS>(pos, moveList, ~pos.pieces(), &ci)
                     : generate_all<BLACK, QUIET_CHECKS>(pos, moveList, ~pos.pieces(), &ci);
}


/// generate<EVASIONS> generates all pseudo-legal check evasions when the side
/// to move is in check. Returns a pointer to the end of the move list.
template<>
ExtMove* generate<EVASIONS>(const Position& pos, ExtMove* moveList) {

  assert(pos.checkers());

  Color us = pos.side_to_move();
  Square ksq = pos.square<KING>(us);
  Bitboard sliderAttacks = 0;
  Bitboard sliders = pos.checkers() & ~pos.pieces(KNIGHT, PAWN);

  // Find all the squares attacked by slider checkers. We will remove them from
  // the king evasions in order to skip known illegal moves, which avoids any
  // useless legality checks later on.
  while (sliders)
  {
      Square checksq = pop_lsb(&sliders);
      sliderAttacks |= LineBB[checksq][ksq] ^ checksq;
  }

  // Generate evasions for king, capture and non capture moves
  Bitboard b = pos.attacks_from<KING>(ksq) & ~pos.pieces(us) & ~sliderAttacks;
  while (b)
      *moveList++ = make_move(ksq, pop_lsb(&b));

  if (more_than_one(pos.checkers()))
      return moveList; // Double check, only a king move can save the day

  // Generate blocking evasions or captures of the checking piece
  Square checksq = lsb(pos.checkers());
  Bitboard target = between_bb(checksq, ksq) | checksq;

  return us == WHITE ? generate_all<WHITE, EVASIONS>(pos, moveList, target)
                     : generate_all<BLACK, EVASIONS>(pos, moveList, target);
}


/// generate<LEGAL> generates all the legal moves in the given position

template<>
ExtMove* generate<LEGAL>(const Position& pos, ExtMove* moveList) {

  Bitboard pinned = pos.pinned_pieces(pos.side_to_move());
  Square ksq = pos.square<KING>(pos.side_to_move());
  ExtMove* cur = moveList;

  moveList = pos.checkers() ? generate<EVASIONS    >(pos, moveList)
                            : generate<NON_EVASIONS>(pos, moveList);
  while (cur != moveList)
      if (   (pinned || from_sq(*cur) == ksq || type_of(*cur) == ENPASSANT)
          && !pos.legal(*cur, pinned))
          *cur = (--moveList)->move;
      else
          ++cur;

  return moveList;
}

#else // #if !defined(CLOWNISH)

namespace {

	/// 指定位置に駒を移動する手の生成
	template <Color C>
	ExtMove* add_moves_to(const Position& pos, ExtMove* emList, Square toSq, IndexMask candidates) {

		for ( ; candidates; lsb_clear(&candidates)) {
			Square fromSq = pos.square_at(lsb_index(candidates));
			Piece piece = pos.piece_on(fromSq);
			if (CAN_PROMOTION(toSq, fromSq, piece))
				(emList++)->move = MOVE_MAKE(toSq, fromSq, piece, pos.piece_on(toSq), 1);
			if (CAN_UNPROMOTION(toSq, piece) || (piece == KING(C) && !pos.effects_on(toSq, ~C)))
				(emList++)->move = MOVE_MAKE(toSq, fromSq, piece, pos.piece_on(toSq), 0);
		}

		return emList;
	}

	/// 指定位置に駒を打つ手の生成
	template <Color C>
	ExtMove* add_drops_to(const Position& pos, ExtMove* emList, Square toSq) {

		if ((pos.pieces_in_hand(C) & HAND_MASK_PAWN)   && (C == BLACK ? toSq >= SQ_9B : toSq <= SQ_1H) && !(pos.exists_pawns(C) & SquareToFileBit[toSq]))
			(emList++)->move = DROP_MAKE(toSq, PAWN(C));
		if ((pos.pieces_in_hand(C) & HAND_MASK_LANCE)  && (C == BLACK ? toSq >= SQ_9B : toSq <= SQ_1H))
			(emList++)->move = DROP_MAKE(toSq, LANCE(C));
		if ((pos.pieces_in_hand(C) & HAND_MASK_KNIGHT) && (C == BLACK ? toSq >= SQ_9C : toSq <= SQ_1G))
			(emList++)->move = DROP_MAKE(toSq, KNIGHT(C));
		if (pos.pieces_in_hand(C) & HAND_MASK_SILVER)
			(emList++)->move = DROP_MAKE(toSq, SILVER(C));
		if (pos.pieces_in_hand(C) & HAND_MASK_GOLD)
			(emList++)->move = DROP_MAKE(toSq, GOLD(C));
		if (pos.pieces_in_hand(C) & HAND_MASK_BISHOP)
			(emList++)->move = DROP_MAKE(toSq, BISHOP(C));
		if (pos.pieces_in_hand(C) & HAND_MASK_ROOK)
			(emList++)->move = DROP_MAKE(toSq, ROOK(C));

		return emList;
	}

	/// 取る手の生成
	template <Color C>
	ExtMove* gen_captures(const Position& pos, ExtMove* emList) {

		for (IndexMask captures = pos.color_mask(~C); captures; ) {
			Index idx = msb_index(captures);
			Square toSq = pos.square_at(idx);
			emList = add_moves_to<C>(pos, emList, toSq, pos.effects_on(toSq, C));
			captures ^= 1ull << idx;
		}

		return emList;
	}

	/// 取らない手の生成
	template <Color C>
	ExtMove* gen_noncaptures(const Position& pos, ExtMove* emList) {

		for (Rank r = RANK_A; r <= RANK_I; ++r) {
			for (File f = FILE_9; f >= FILE_1; --f) {
				Square toSq = (C == BLACK ? SQ_MAKE(f, r) : SQ_INV(SQ_MAKE(f, r))); // 座標反転は要らないかも…
				if (pos.piece_on(toSq) == EMPTY) {
					emList = add_moves_to<C>(pos, emList, toSq, pos.effects_on(toSq, C));
					emList = add_drops_to<C>(pos, emList, toSq);
				}
			}
		}

		return emList;
	}


	/// 王手回避手の生成
	template <Color C>
	ExtMove* gen_evasions(const Position& pos, ExtMove* emList) {

		Square kingSq = pos.king_square(C);

		IndexMask checkerBits = pos.effects_on(kingSq, ~C);
		ASSERT_DBG(checkerBits != 0);

		// 王手駒の位置
		Square checkerSq1 = pos.square_at(lsb_index(checkerBits));
		Square checkerSq2 = pos.square_at(msb_index(checkerBits));

		// 王手駒の王手方向（桂馬王手 == 0）
		Square checkerInc1 = DELTA_TO_INC(kingSq - checkerSq1);
		Square checkerInc2 = DELTA_TO_INC(kingSq - checkerSq2);

		// 王手駒を取る手
		if (checkerSq1 == checkerSq2)
			emList = add_moves_to<C>(pos, emList, checkerSq1, pos.effects_on(checkerSq1, C) & ~INDEX_MASK_KING);

		// 玉が移動する手
		for (int i = 0; i < 8; ++i) {

			Square inc = DirectionToInc[i];
			if (   (inc == checkerInc1 && ((1 << pos.piece_on(checkerSq1)) & DELTA_SLIDING_PIECES(kingSq - checkerSq1)))
			    || (inc == checkerInc2 && ((1 << pos.piece_on(checkerSq2)) & DELTA_SLIDING_PIECES(kingSq - checkerSq2))))
				continue; // 利きの貫き

			Square toSq = kingSq + inc;
			if (CAN_CAPTURE(pos.piece_on(toSq), C) && !pos.effects_on(toSq, ~C))
				(emList++)->move = MOVE_MAKE(toSq, kingSq, KING(C), pos.piece_on(toSq), 0);
		}

		// 合い駒する手
		if (checkerSq1 == checkerSq2) {
			for (Square toSq = kingSq; pos.piece_on(toSq -= checkerInc1) == EMPTY; )
				emList = add_moves_to<C>(pos, emList, toSq, pos.effects_on(toSq, C) & ~INDEX_MASK_KING);
			for (Square toSq = kingSq; pos.piece_on(toSq -= checkerInc1) == EMPTY; )
				emList = add_drops_to<C>(pos, emList, toSq);
		}

		return emList;
	}

} // namespace


/// generate<CAPTURES> generates all pseudo-legal captures.
/// Returns a pointer to the end of the move list.
///
/// generate<QUIETS> generates all pseudo-legal non-captures.
/// Returns a pointer to the end of the move list.
///
/// generate<NON_EVASIONS> generates all pseudo-legal captures and non-captures.
/// Returns a pointer to the end of the move list.
///
/// generate<EVASIONS> generates all pseudo-legal check evasions when the side to move is in check.
/// Returns a pointer to the end of the move list.

template<GenType Type>
ExtMove* generate(const Position& pos, ExtMove* moveList) {

	assert(Type == CAPTURES || Type == QUIETS || Type == EVASIONS || Type == NON_EVASIONS);

	Color us = pos.side_to_move();

	if (Type == CAPTURES || Type == NON_EVASIONS)
		moveList = (us == BLACK) ? gen_captures<BLACK>(pos, moveList)
		                         : gen_captures<WHITE>(pos, moveList);

	if (Type == QUIETS || Type == NON_EVASIONS)
		moveList = (us == BLACK) ? gen_noncaptures<BLACK>(pos, moveList)
		                         : gen_noncaptures<WHITE>(pos, moveList);

	if (Type == EVASIONS)
		moveList = (us == BLACK) ? gen_evasions<BLACK>(pos, moveList)
		                         : gen_evasions<WHITE>(pos, moveList);

	if (Type == QUIET_CHECKS)
		; // 未実装

	return moveList;
}

// Explicit template instantiations
template ExtMove* generate<CAPTURES>(const Position&, ExtMove*);
template ExtMove* generate<QUIETS>(const Position&, ExtMove*);
template ExtMove* generate<QUIET_CHECKS>(const Position&, ExtMove*);
template ExtMove* generate<EVASIONS>(const Position&, ExtMove*);
template ExtMove* generate<NON_EVASIONS>(const Position&, ExtMove*);

/// generate<LEGAL> generates all the legal moves in the given position

template<>
ExtMove* generate<LEGAL>(const Position& pos, ExtMove* moveList) {

	ExtMove* cur = moveList;

	moveList = pos.is_in_check() ? generate<EVASIONS    >(pos, moveList)
	                             : generate<NON_EVASIONS>(pos, moveList);

	// 打ち歩詰めをどうするか…
	while (cur != moveList) {
		if (!pos.move_is_valid(*cur))
			*cur = (--moveList)->move;
		else
			++cur;
	}

	return moveList;
}

#endif
