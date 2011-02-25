/************************************************************************
 ************************************************************************
    FAUST compiler
    Copyright (C) 2003-2011 GRAME, Centre National de Creation Musicale
    ---------------------------------------------------------------------
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 ************************************************************************
 ************************************************************************/

#ifndef _MULTIRATE_INSTRUCTION_COMPILER_HH
#define _MULTIRATE_INSTRUCTION_COMPILER_HH

#include "instructions_compiler_base.hh"

using namespace std;

class MultirateInstructionsCompiler:
    public InstructionsCompilerBase
{
public:
    MultirateInstructionsCompiler(CodeContainer* container):
        InstructionsCompilerBase(container)
    {}

private:
    void compileTop(Tree rootSignal);
    void compileVector(VectorAddress * vec, Tree sig);
    StatementInst * compileAssignment(Address * vec, Tree sig, ValueInst * index);

    void compileMultiSignal(Tree rootSignal);
    ValueInst * compileSample(Tree sig, ValueInst * index);

    // signal-specific
    ValueInst * compileSampleInput(Tree sig, int i, ValueInst * index);
    ValueInst * compileSamplePrimitive(Tree sig, ValueInst * index);
    ValueInst * compilePrimitive(Tree sig, ValueInst * index);

    // helper functions
    StatementInst * store (Address * address, ValueInst * value);

    ValueInst * fVectorSize;
};

#endif
