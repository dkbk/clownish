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

#include <fstream>
#include <iostream>
#include <istream>
#include <vector>

#include "misc.h"
#include "position.h"
#include "search.h"
#include "thread.h"
#include "uci.h"
#if defined(CLOWNISH)
#include "evaluate.h"
#endif

using namespace std;

namespace {

#if !defined(CLOWNISH)
const vector<string> Defaults = {
  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
  "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 10",
  "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 11",
  "4rrk1/pp1n3p/3q2pQ/2p1pb2/2PP4/2P3N1/P2B2PP/4RRK1 b - - 7 19",
  "rq3rk1/ppp2ppp/1bnpb3/3N2B1/3NP3/7P/PPPQ1PP1/2KR3R w - - 7 14",
  "r1bq1r1k/1pp1n1pp/1p1p4/4p2Q/4Pp2/1BNP4/PPP2PPP/3R1RK1 w - - 2 14",
  "r3r1k1/2p2ppp/p1p1bn2/8/1q2P3/2NPQN2/PPP3PP/R4RK1 b - - 2 15",
  "r1bbk1nr/pp3p1p/2n5/1N4p1/2Np1B2/8/PPP2PPP/2KR1B1R w kq - 0 13",
  "r1bq1rk1/ppp1nppp/4n3/3p3Q/3P4/1BP1B3/PP1N2PP/R4RK1 w - - 1 16",
  "4r1k1/r1q2ppp/ppp2n2/4P3/5Rb1/1N1BQ3/PPP3PP/R5K1 w - - 1 17",
  "2rqkb1r/ppp2p2/2npb1p1/1N1Nn2p/2P1PP2/8/PP2B1PP/R1BQK2R b KQ - 0 11",
  "r1bq1r1k/b1p1npp1/p2p3p/1p6/3PP3/1B2NN2/PP3PPP/R2Q1RK1 w - - 1 16",
  "3r1rk1/p5pp/bpp1pp2/8/q1PP1P2/b3P3/P2NQRPP/1R2B1K1 b - - 6 22",
  "r1q2rk1/2p1bppp/2Pp4/p6b/Q1PNp3/4B3/PP1R1PPP/2K4R w - - 2 18",
  "4k2r/1pb2ppp/1p2p3/1R1p4/3P4/2r1PN2/P4PPP/1R4K1 b - - 3 22",
  "3q2k1/pb3p1p/4pbp1/2r5/PpN2N2/1P2P2P/5PP1/Q2R2K1 b - - 4 26",
  "6k1/6p1/6Pp/ppp5/3pn2P/1P3K2/1PP2P2/3N4 b - - 0 1",
  "3b4/5kp1/1p1p1p1p/pP1PpP1P/P1P1P3/3KN3/8/8 w - - 0 1",
  "2K5/p7/7P/5pR1/8/5k2/r7/8 w - - 0 1",
  "8/6pk/1p6/8/PP3p1p/5P2/4KP1q/3Q4 w - - 0 1",
  "7k/3p2pp/4q3/8/4Q3/5Kp1/P6b/8 w - - 0 1",
  "8/2p5/8/2kPKp1p/2p4P/2P5/3P4/8 w - - 0 1",
  "8/1p3pp1/7p/5P1P/2k3P1/8/2K2P2/8 w - - 0 1",
  "8/pp2r1k1/2p1p3/3pP2p/1P1P1P1P/P5KR/8/8 w - - 0 1",
  "8/3p4/p1bk3p/Pp6/1Kp1PpPp/2P2P1P/2P5/5B2 b - - 0 1",
  "5k2/7R/4P2p/5K2/p1r2P1p/8/8/8 b - - 0 1",
  "6k1/6p1/P6p/r1N5/5p2/7P/1b3PP1/4R1K1 w - - 0 1",
  "1r3k2/4q3/2Pp3b/3Bp3/2Q2p2/1p1P2P1/1P2KP2/3N4 w - - 0 1",
  "6k1/4pp1p/3p2p1/P1pPb3/R7/1r2P1PP/3B1P2/6K1 w - - 0 1",
  "8/3p3B/5p2/5P2/p7/PP5b/k7/6K1 w - - 0 1",

  // 5-man positions
  "8/8/8/8/5kp1/P7/8/1K1N4 w - - 0 1",     // Kc2 - mate
  "8/8/8/5N2/8/p7/8/2NK3k w - - 0 1",      // Na2 - mate
  "8/3k4/8/8/8/4B3/4KB2/2B5 w - - 0 1",    // draw

  // 6-man positions
  "8/8/1P6/5pr1/8/4R3/7k/2K5 w - - 0 1",   // Re5 - mate
  "8/2p4P/8/kr6/6R1/8/8/1K6 w - - 0 1",    // Ka2 - mate
  "8/8/3P3k/8/1p6/8/1P6/1K3n2 b - - 0 1",  // Nd2 - draw

  // 7-man positions
  "8/R7/2q5/8/6k1/8/1P5p/K6R w - - 0 124"  // Draw
};
#else
const vector<string> Defaults = {
	"lR1B3nl/2gp5/ngk1+BspPp/1s2p2p1/p4S3/1Pp6/P5P1P/LGG6/KN5NL b Prs5p 1",
	"5S2l/1rP2s1k1/p2+B1gnp1/5np2/3G3n1/5S2p/P1+p1PpPP1/1P1PG2KP/L2+rLPGNL b Bs3p 1",
	"lR6l/1s1g5/1k1s1+P2p/1+bpp1+Bs2/1n1n2Pp1/2P6/S2R4P/K1GG5/9 b 2NPg2l9p 1",
	"l4g1nl/4g1k2/2n1sp1p1/p5pPp/5Ps2/1P1p2s2/P1G1+p1N1P/6K2/LN5RL b RBG3Pbs3p 1",
	"1n4g1k/6r2/1+P1psg1p+L/2p1pp3/3P5/p1P1PPPP1/3SGS3/1+p1K1G2r/9 b 2BNLPs2n2l3p 1",
	"+B2+R3n1/3+L2gk1/5gss1/p1p1p1ppl/5P2p/PPPnP1PP1/3+p2N2/6K2/L4S1RL b BGS3Pgnp 1",
	"3R4l/1kg6/2ns5/spppp2+Bb/p7p/1PPPP1g2/nSNSNP2P/KG1G5/5r2L b L4Pl2p 1",
	"ln5nl/2r2gk2/1p2sgbpp/pRspppp2/L1p4PP/3PP1G2/N4PP2/3BS1SK1/5G1NL b 3P 1",
	"ln7/1r2k1+P2/p3gs3/1b1g1p+B2/1p5R1/2pPP4/PP1S1P3/2G2G3/LN1K5 b SNL3Psnl5p 1",
	"3+P3+Rl/2+P2kg2/+B2psp1p1/4p1p1p/9/2+p1P+bnKP/P6P1/4G1S2/L4G2L b G2S2NLrn5p 1",
	"ln1gb2nl/1ks4r1/1p1g4p/p1pppspB1/5p3/PPPPP1P2/1KNG1PS1P/2S4R1/L2G3NL b Pp 1",
	"lr6l/4g1k1p/1s1p1pgp1/p3P1N1P/2Pl5/PPbBSP3/6PP1/4S1SK1/1+r3G1NL b N3Pgn2p 1",
	"l1ks3+Bl/2g2+P3/p1npp4/1sNn2B2/5p2p/2PP5/PPN1P1+p1P/1KSSg2P1/L1G5+r b GL4Pr 1",
	"ln3k1nl/2P1g4/p1lpsg1pp/4p4/1p1P1p3/2SBP4/PP1G1P1PP/1K1G3+r1/LN1s2PNR b BSPp 1",
	"+N6nl/1+R2pGgk1/5Pgp1/p2p1sp2/3B1p2p/P1pP4P/6PP1/L3G1K2/7NL b RNL2Pb3s2p 1",
	"ln1g5/1r4k2/p2pppn2/2ps2p2/1p7/2P6/PPSPPPPLP/2G2K1pr/LN4G1b b BG2SLPnp 1",
	"ln3k2l/1r7/p1bp1g1pp/2p1p4/1pBP1ns2/4Pp3/PPSG3PP/1KG2R3/LN5NL b G2P2s2p 1",
	"ln6l/1r4gk1/p2psg1pp/2pb1pp2/1p2p1Ss1/2PP4P/PPSG1P3/2GB2R2/LNK5L b NPn2p 1",
	"ln1g5/1ks4+Rl/1pbg2+P2/pn2r1s1p/2Pp1P3/P3PS2P/1PGP1G3/1K1s5/LN6L b BN2P4p 1",
	"ln1g5/1ks5l/1pb6/1n2r1+P+Rp/2PpPP3/pGn2S2P/1P1P1G3/K2s5/LN6L b BGSP6p 1",
	"ln3g1nl/4g1s2/p1+Pp1p1k1/6ppp/3P5/2p1pPPPP/PP5R1/4G1SK1/LN1+r1G1NL b BSPbsp 1",
	"5g1+Ll/4g1s2/p4p2k/3p2pPp/3P1N1p1/2pbpPS1P/P+r5S1/4G3K/L3PG1NL b S2Nrb4p 1",
	"l+B6l/3+P2gk1/2p2g1s1/p2Gppp1p/1p5n1/P1+b2PPSP/2N2SNK1/6GS1/7rL b RN4Pl2p 1",
	"l+B6l/3+PRn1k1/2p2g1g1/p2Gpppp1/1p5Lp/P4PP1P/2+r4P1/6GSK/4P3L b BS2N3P2sn 1",
	"3+Rl2kl/S1S1+PG3/2+b1p1B2/7rp/1Kp2pPN1/6GPP/1P2P3L/9/L5+p2 b 2N5P2g2sn2p 1",
	"4pg1nl/2p2gkb1/p1n+RP2p1/r4pp1p/Bp7/3P1P2P/PP4PP1/2+p3SK1/LN3G1NL b 2Sgslp 1",
	"ln1g2+Rn1/1ks1g3l/1p2s2p1/p1pp1B2p/7P1/P1PSp3P/1P1P1G+p2/1K3P3/LN5+rL b BGS2Pnp 1",
	"lr6l/3+P+P1gS1/5k3/2pGs1spp/p3p4/1PPR1p2P/PGG3+b2/1K4+b2/LNN5L b S3P2n3p 1",
	"l2g4+B/2s6/2kn3+Rp/ppg1p1P2/1ns5b/2ppP4/PP1P1P1PP/LSGS5/KNG4+rL b NLP3p 1",
	"l3l1pn1/4k4/3ppg1+Rl/p2s5/9/P2+R2P2/1P1PPP3/2K1+p4/L6N1 b G3S2N3P2b2g4p 1",
	"ln5nl/2+R2sk2/pp5pp/4pb3/4Ppp2/6PP1/Psp2PB1P/3PS2R1/L1K4NL b 4GNPs2p 1",
	"l1l1B3l/6g2/4p1nk1/4spgpp/3p2B2/rpG1S1p1P/N3P1N2/PKG6/7R1 b SNLs8p 1",
	"lngg2+R2/1k4P1l/1ps2+P1pp/1nppp4/p6P1/P1P1P1S2/BP1P1S2P/2KG1+b1+r1/LN1N4L b GS2P 1",
	"l2r1g1nl/6sk1/5g3/3Ps1ppp/pRB2p3/2P1p1PPP/P2G1PSS1/6GK1/+b6NL b N4Pnlp 1",
	"l+b1r1n2l/1p1p2sk1/2+R2gnl1/3G2p1p/p1B1pp1P1/2P3P1P/P3GPSS1/6GK1/7NL b 5Psn 1",
	"l2r4l/1+R1pg1sk1/5gnl1/3P+b1ppp/p1B1pp3/2P3PPP/P3GPSS1/6GK1/7NL b S4P2n 1",
	"l2r1n2l/1p1p3k1/5g1l1/2+R3p1p/p3pp3/6PPP/P3GPNS1/6GK1/8L b BSN3Pbg2sn3p 1",
	"ln1g3nl/1k3rg2/4s1bpp/1pPsp4/2Bp2SP1/p3P4/1PNP1P2P/2KGG1R2/L6NL b S2P3p 1",
	"l2g3nl/6g2/1k1+N3pp/1pp1p4/2+rB2SP1/p3P4/1P1P1P2P/3G2R2/Ls2K2NL b 2SNbg6p 1",
	"ln1g4l/1ksg1+P3/ppppp2pp/1l3N3/9/1BPPPS2B/PP1G4P/1KS2+n3/LN1G2+r1+r b SP3p 1",
	"lnS+N4l/9/ksppp2pp/pp4p2/9/1PPPPS2B/P2G4P/1KS2+n3/LN1G1P+r1+r b GLbg3p 1",
	"lR4Bnl/5gs2/5nkpp/p1P1psp2/3P5/4P1PPP/P+p3G3/3+b2SK1/L4G1NL b G4Prsnp 1",
	"l1+L6/1k1rn2+R1/p1p1ppg2/1G1P1bp1p/2P6/2Ns1N2P/PPNpPP3/1SG1K1S2/L4G2L b 2Pbs2p 1",
	"l5k1l/4+Psg2/p1r2p1p1/4s1p1p/1p3n3/P3P4/BP3PP1P/2GS3R1/+bNK4NL b GNPgsl4p 1",
	"ln1g4l/1kss2g2/2pp1pn2/6rp1/PpPPP1pPp/p3SP3/BPS2GP1L/1KG3R2/LN5N1 b BPp 1",
	"l2g2ks1/4P4/2p1+S2pn/p2p2+r1p/5+B3/P3S2PP/1PPP+b1P2/1rG2P3/LN2KG1NL b GN2Psl2p 1",
	"l7l/4+N1+N1k/2+B1p1ng1/p5ppp/2P1S4/PpSp2P1P/2S1P4/1KG4R1/LN6+r b B4P2gsl2p 1",
	"7nl/5bgk1/2+Pp1gspp/P4p3/4P1PP1/1l1P5/2NG1+s2P/1g1S3R1/L2KB2NL b RSN6Pp 1",
};
#endif

} // namespace

/// benchmark() runs a simple benchmark by letting Stockfish analyze a set
/// of positions for a given limit each. There are five parameters: the
/// transposition table size, the number of search threads that should
/// be used, the limit value spent for each position (optional, default is
/// depth 13), an optional file name where to look for positions in FEN
/// format (defaults are the positions defined above) and the type of the
/// limit value: depth (default), time in millisecs or number of nodes.

void benchmark(const Position& current, istream& is) {

  string token;
  vector<string> fens;
  Search::LimitsType limits;

  // Assign default values to missing arguments
  string ttSize    = (is >> token) ? token : "16";
  string threads   = (is >> token) ? token : "1";
  string limit     = (is >> token) ? token : "13";
  string fenFile   = (is >> token) ? token : "default";
  string limitType = (is >> token) ? token : "depth";

  Options["Hash"]    = ttSize;
  Options["Threads"] = threads;
  Search::clear();

#if defined(CLOWNISH)
  evaluate_load();
#endif

  if (limitType == "time")
      limits.movetime = stoi(limit); // movetime is in millisecs

  else if (limitType == "nodes")
      limits.nodes = stoi(limit);

  else if (limitType == "mate")
      limits.mate = stoi(limit);

  else
      limits.depth = stoi(limit);

  if (fenFile == "default")
      fens = Defaults;

  else if (fenFile == "current")
#if !defined(CLOWNISH)
      fens.push_back(current.fen());
#else
      fens.push_back(current.fen(1));
#endif

  else
  {
      string fen;
      ifstream file(fenFile);

      if (!file.is_open())
      {
          cerr << "Unable to open file " << fenFile << endl;
          return;
      }

      while (getline(file, fen))
          if (!fen.empty())
              fens.push_back(fen);

      file.close();
  }

  uint64_t nodes = 0;
  TimePoint elapsed = now();

  for (size_t i = 0; i < fens.size(); ++i)
  {
#if !defined(CLOWNISH)
      Position pos(fens[i], Options["UCI_Chess960"], Threads.main());
#else
      Position pos(fens[i], Threads.main());
#endif

      cerr << "\nPosition: " << i + 1 << '/' << fens.size() << endl;

      if (limitType == "perft")
          nodes += Search::perft(pos, limits.depth * ONE_PLY);

      else
      {
          Search::StateStackPtr st;
          limits.startTime = now();
          Threads.start_thinking(pos, limits, st);
          Threads.main()->wait_for_search_finished();
          nodes += Threads.nodes_searched();
      }
  }

  elapsed = now() - elapsed + 1; // Ensure positivity to avoid a 'divide by zero'

  dbg_print(); // Just before exiting

  cerr << "\n==========================="
       << "\nTotal time (ms) : " << elapsed
       << "\nNodes searched  : " << nodes
       << "\nNodes/second    : " << 1000 * nodes / elapsed << endl;
}
