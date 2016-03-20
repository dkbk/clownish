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

#include <iostream>

#if !defined(CLOWNISH)
#include "bitboard.h"
#include "evaluate.h"
#endif
#include "position.h"
#include "search.h"
#include "thread.h"
#include "tt.h"
#include "uci.h"
#if !defined(CLOWNISH)
#include "syzygy/tbprobe.h"
#else
#include "mate.h"
#endif

int main(int argc, char* argv[]) {

  std::cout << engine_info() << std::endl;

#if !defined(CLOWNISH)
  UCI::init(Options);
  PSQT::init();
  Bitboards::init();
  Position::init();
  Bitbases::init();
  Search::init();
  Eval::init();
  Pawns::init();
  Threads.init();
  Tablebases::init(Options["SyzygyPath"]);
  TT.resize(Options["Hash"]);
#else
  UCI::init(Options);
  Position::init();
  mate_init();
  Search::init();
  Threads.init();
  TT.resize(Options["Hash"]);
#endif

  UCI::loop(argc, argv);

  Threads.exit();
  return 0;
}
