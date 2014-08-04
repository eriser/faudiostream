#ifndef SIGTOVHDL_HH
#define SIGTOVHDL_HH

#include "signals.hh"
#include <string>
#include <iostream>
#include <fstream>
#include <vector>
#include <set>
#include <map>
#include <queue>

#include "flopoco/FloPoCo.hpp"

using namespace std;

/**
 * Produce synthtizable VHDL using the flopoco framework
 */

void sigToVHDL (Tree sig, ofstream& fout);

namespace flopoco 
{
	/**
	 * Class for vhdl building, derived from the flopoco framework.
	 */
	class FlopOp: public Operator 
	{
		public:
		FlopOp(Target *t, Tree sig, ofstream& fout, const map<Tree, int> *ppLs, const map<Tree, list<Tree> > *nodeFathers, map<string, double> inputDelays = emptyDelayMap);
		~FlopOp();
	};

}
#endif // SIGTOVHDL_HH
