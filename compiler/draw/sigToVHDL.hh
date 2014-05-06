#ifndef SIGTOVHDL_HH
#define SIGTOVHDL_HH

#include "signals.hh"
#include <string>
#include <iostream>
#include <fstream>

using namespace std;

/**
 * Produce synthtizable VHDL using the flopoco framework
 */
void sigToVHDL (Tree sig, ofstream& fout);

#endif // SIGTOVHDL_HH
