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

#include "book.h"
#include "misc.h"
#include "position.h"

#include <unordered_map>
#include <vector>
#include <iostream>
#include <fstream>
#include <sstream>


namespace {

struct BookMove {
	Move move;
	int count;
};

std::unordered_map<std::string, std::vector<BookMove>> Book;

PRNG bookprng((uint64_t)now);

/// 定跡の内部登録
void book_parse(std::ifstream& ifs) {

	Position pos;
	std::string line, token;

	while (std::getline(ifs, line)) {

		if (line.compare(0, 5, "sfen ") != 0)
			continue;

		size_t separate = line.find(" moves ", 7);
		if (separate == std::string::npos)
			continue;

		std::string sfen(line.substr(5, separate - 5));

		pos.set(sfen, nullptr);

		std::stringstream ss(line.substr(separate + 7));

		while (ss >> token) {
			BookMove bm;
			bm.move = move_from_usi(token, pos);
			ss >> bm.count;

			Book[sfen].push_back(bm);
		}
	}
}

} // namespace

/// 定跡の読み込み
void book_load() {

	if (!Book.empty())
		return;

	// 棋譜から抽出した定跡
	std::ifstream bookN("./book.sfen");
	if (bookN)
		book_parse(bookN);
	else
		std::cerr << "failed : open 'book.sfen'" << std::endl;
}

/// 定跡の参照 ※ 出現頻度をもとに指し手を選択
Move book_retrieve(const Position& pos) {

	auto it = Book.find(pos.fen(1));
	if (it == Book.end())
		return MOVE_NONE;
	if (it->second.size() == 1)
		return it->second[0].move;

	int sum = 0;
	for (auto& bm : it->second)
		sum += bm.count;

	int r = bookprng.rand<int>() % sum;
	for (auto& bm : it->second) {
		if ((r -= bm.count) < 0)
			return bm.move;
	}

	return MOVE_NONE;
}
