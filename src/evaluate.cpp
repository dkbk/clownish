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

#include "evaluate.h"
#include "position.h"

#include <iostream>
#include <fstream>


// KKP
enum {
	KKP_HAND_PAWN   = 0,
	KKP_HAND_LANCE  = KKP_HAND_PAWN   + 19,
	KKP_HAND_KNIGHT = KKP_HAND_LANCE  +  5,
	KKP_HAND_SILVER = KKP_HAND_KNIGHT +  5,
	KKP_HAND_GOLD   = KKP_HAND_SILVER +  5,
	KKP_HAND_BISHOP = KKP_HAND_GOLD   +  5,
	KKP_HAND_ROOK   = KKP_HAND_BISHOP +  3,
	KKP_PAWN        = KKP_HAND_ROOK   +  3,
	KKP_LANCE       = KKP_PAWN        + 81,
	KKP_KNIGHT      = KKP_LANCE       + 81,
	KKP_SILVER      = KKP_KNIGHT      + 81,
	KKP_GOLD        = KKP_SILVER      + 81,
	KKP_BISHOP      = KKP_GOLD        + 81,
	KKP_PROM_BISHOP = KKP_BISHOP      + 81,
	KKP_ROOK        = KKP_PROM_BISHOP + 81,
	KKP_PROM_ROOK   = KKP_ROOK        + 81,
	KKP_NB          = KKP_PROM_ROOK   + 81,
};


short kkp[81][81][KKP_NB];


#define DPawn            93 /*  186 */
#define DKnight         240 /*  480 */
#define DLance          243 /*  486 */
#define DProPawn        560 /*  653 */
#define DProLance       448 /*  691 */
#define DProKnight      496 /*  736 */
#define DSilver         385 /*  770 */
#define DProSilver      450 /*  835 */
#define DGold           461 /*  922 */
#define DBishop         562 /* 1124 */
#define DRook           671 /* 1342 */
#define DHorse          804 /* 1366 */
#define DDragon         987 /* 1658 */
#define DKing         15000


static const short IndexKKP[32] = {
	0, KKP_PAWN, KKP_LANCE, KKP_KNIGHT, KKP_SILVER, KKP_GOLD, KKP_BISHOP, KKP_ROOK, 0, KKP_GOLD, KKP_GOLD, KKP_GOLD, KKP_GOLD, 0, KKP_PROM_BISHOP, KKP_PROM_ROOK,
	0, KKP_PAWN, KKP_LANCE, KKP_KNIGHT, KKP_SILVER, KKP_GOLD, KKP_BISHOP, KKP_ROOK, 0, KKP_GOLD, KKP_GOLD, KKP_GOLD, KKP_GOLD, 0, KKP_PROM_BISHOP, KKP_PROM_ROOK,
};

int ValuePieceMg[48] = { // Bonanza
	0,  DPawn,  DLance,  DKnight,  DSilver,  DGold,  DBishop,  DRook,  DKing,  DProPawn,  DProLance,  DProKnight,  DProSilver, 0,  DHorse,  DDragon,
	0,  DPawn,  DLance,  DKnight,  DSilver,  DGold,  DBishop,  DRook,  DKing,  DProPawn,  DProLance,  DProKnight,  DProSilver, 0,  DHorse,  DDragon,
	0, -DPawn, -DLance, -DKnight, -DSilver, -DGold, -DBishop, -DRook, -DKing, -DProPawn, -DProLance, -DProKnight, -DProSilver, 0, -DHorse, -DDragon,
};

int ValueCaptureMg[48] = { // Bonanza
	0,  (DPawn+DPawn),  (DLance+DLance),  (DKnight+DKnight),  (DSilver+DSilver),  (DGold+DGold),  (DBishop+DBishop),  (DRook+DRook),  (DKing+DKing),  (DProPawn+DPawn),  (DProLance+DLance),  (DProKnight+DKnight),  (DProSilver+DSilver), 0,  (DHorse+DBishop),  (DDragon+DRook),
	0,  (DPawn+DPawn),  (DLance+DLance),  (DKnight+DKnight),  (DSilver+DSilver),  (DGold+DGold),  (DBishop+DBishop),  (DRook+DRook),  (DKing+DKing),  (DProPawn+DPawn),  (DProLance+DLance),  (DProKnight+DKnight),  (DProSilver+DSilver), 0,  (DHorse+DBishop),  (DDragon+DRook),
	0, -(DPawn+DPawn), -(DLance+DLance), -(DKnight+DKnight), -(DSilver+DSilver), -(DGold+DGold), -(DBishop+DBishop), -(DRook+DRook), -(DKing+DKing), -(DProPawn+DPawn), -(DProLance+DLance), -(DProKnight+DKnight), -(DProSilver+DSilver), 0, -(DHorse+DBishop), -(DDragon+DRook),
};

int ValuePromotionMg[48] = { // Bonanza
	0,  (DProPawn-DPawn),  (DProLance-DLance),  (DProKnight-DKnight),  (DProSilver-DSilver), 0,  (DHorse-DBishop),  (DDragon-DRook), 0, 0, 0, 0, 0, 0, 0, 0,
	0,  (DProPawn-DPawn),  (DProLance-DLance),  (DProKnight-DKnight),  (DProSilver-DSilver), 0,  (DHorse-DBishop),  (DDragon-DRook), 0, 0, 0, 0, 0, 0, 0, 0,
	0, -(DProPawn-DPawn), -(DProLance-DLance), -(DProKnight-DKnight), -(DProSilver-DSilver), 0, -(DHorse-DBishop), -(DDragon-DRook), 0, 0, 0, 0, 0, 0, 0, 0,
};


static bool EvaluateBinariesLoaded = false;

/// 初期化
bool evaluate_load() {

	if (EvaluateBinariesLoaded)
		return true;

	std::ifstream ifs("./fv_kkp.bin", std::ios::in | std::ios::binary);
	if (!ifs) {
		std::cerr << "failed : open 'fv_kkp.bin'" << std::endl;
		return false;
	}

	ifs.read((char *)kkp, sizeof(short) * (81 * 81 * KKP_NB));
	if (!ifs) {
		std::cerr << "failed : read 'fv_kkp.bin'" << std::endl;
		return false;
	}

	EvaluateBinariesLoaded = true;

	return true;
}

/// 局面評価
Value evaluate(const Position& pos) {

	int score = 0;

	const Square bk    = SquareTo81[pos.king_square(BLACK)];
	const Square wk    = SquareTo81[pos.king_square(WHITE)];
	const Square bkInv = SQ81_INV(bk);
	const Square wkInv = SQ81_INV(wk);

	const int bHand = pos.pieces_in_hand(BLACK);
	const int wHand = pos.pieces_in_hand(WHITE);

	// 駒台

	score += kkp[bk   ][wk   ][KKP_HAND_PAWN   + HAND_TO_PAWN(bHand)];
	score += kkp[bk   ][wk   ][KKP_HAND_LANCE  + HAND_TO_LANCE(bHand)];
	score += kkp[bk   ][wk   ][KKP_HAND_KNIGHT + HAND_TO_KNIGHT(bHand)];
	score += kkp[bk   ][wk   ][KKP_HAND_SILVER + HAND_TO_SILVER(bHand)];
	score += kkp[bk   ][wk   ][KKP_HAND_GOLD   + HAND_TO_GOLD(bHand)];
	score += kkp[bk   ][wk   ][KKP_HAND_BISHOP + HAND_TO_BISHOP(bHand)];
	score += kkp[bk   ][wk   ][KKP_HAND_ROOK   + HAND_TO_ROOK(bHand)];

	score -= kkp[wkInv][bkInv][KKP_HAND_PAWN   + HAND_TO_PAWN(wHand)];
	score -= kkp[wkInv][bkInv][KKP_HAND_LANCE  + HAND_TO_LANCE(wHand)];
	score -= kkp[wkInv][bkInv][KKP_HAND_KNIGHT + HAND_TO_KNIGHT(wHand)];
	score -= kkp[wkInv][bkInv][KKP_HAND_SILVER + HAND_TO_SILVER(wHand)];
	score -= kkp[wkInv][bkInv][KKP_HAND_GOLD   + HAND_TO_GOLD(wHand)];
	score -= kkp[wkInv][bkInv][KKP_HAND_BISHOP + HAND_TO_BISHOP(wHand)];
	score -= kkp[wkInv][bkInv][KKP_HAND_ROOK   + HAND_TO_ROOK(wHand)];

	// 盤上

	IndexMask bp = pos.color_mask(BLACK) & ~INDEX_MASK_KING;
	for ( ; bp; lsb_clear(&bp)) {
		Square sq = pos.square_at(lsb_index(bp));
		score += kkp[bk   ][wk   ][IndexKKP[pos.piece_on(sq)] + SquareTo81[sq]];
	}
	IndexMask wp = pos.color_mask(WHITE) & ~INDEX_MASK_KING;
	for ( ; wp; lsb_clear(&wp)) {
		Square sq = pos.square_at(lsb_index(wp));
		score -= kkp[wkInv][bkInv][IndexKKP[pos.piece_on(sq)] + SquareToInv81[sq]];
	}

	score += pos.material_score() * FV_SCALE;

	return static_cast<Value>(pos.black_to_move() ? score / FV_SCALE : -score / FV_SCALE);
}
