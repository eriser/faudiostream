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

using namespace std;

/**
 * Produce instructions to build vhdl pipeline with the flopoco framework
 */

void sigToVHDL (Tree sig, ofstream& fout);

#endif // SIGTOVHDL_HH
