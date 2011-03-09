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
    void compileVector(NamedAddress * vec, Tree sig);
    StatementInst * compileAssignment(Address * vec, Tree sig, FIRIndex const & index);

    void compileMultiSignal(Tree rootSignal);
    ValueInst * compileSample(Tree sig, FIRIndex const & index);

    // signal-specific
    ValueInst * compileSampleInput(Tree sig, int i, FIRIndex const & index);
    ValueInst * compileSampleVectorize(Tree sig, FIRIndex const & index, Tree arg1, Tree arg2);
    ValueInst * compileSampleSerialize(Tree sig, FIRIndex const & index, Tree arg1);
    ValueInst * compileSampleConcat(Tree sig, FIRIndex const & index, Tree arg1, Tree arg2);
    ValueInst * compileSampleAt(Tree sig, FIRIndex const & index, Tree arg1, Tree arg2);
    ValueInst * compileSamplePrimitive(Tree sig, FIRIndex const & index);
    ValueInst * compilePrimitive(Tree sig, FIRIndex const & index);
    ValueInst * compileSampleDelay(Tree sig, FIRIndex const & index, Tree delayline, Tree delay);

    ValueInst * compileBinop(Tree sig, int opcode, Tree arg1, Tree arg2, FIRIndex const & index);

    StatementInst * compileAssignmentVectorize(Address * vec, Tree sig, FIRIndex const & index, Tree arg1, Tree arg2);
    StatementInst * compileAssignmentSerialize(Address * vec, Tree sig, FIRIndex const & index, Tree arg1);
    StatementInst * compileAssignmentConcat(Address * vec, Tree sig, FIRIndex const & index, Tree arg1, Tree arg2);
    StatementInst * compileAssignmentAt(Address * vec, Tree sig, FIRIndex const & index, Tree arg1, Tree arg2);

    Address * compileDelayline(Tree delayline);
    Address * declareDelayLine(Tree delayline);

    // helper functions
    StatementInst * store (Address * address, ValueInst * value);
    LoadVarInst * loadCount(void)
    {
        return InstBuilder::genLoadStackVar("count");
    }

    Typed * declareSignalType(Tree sig);
    Typed * declareSignalType(AudioType * type);
    ArrayTyped * declareArrayTyped(Typed * typed, int size);

    ForLoopInst* genSubloop(string const & loopSymbol, int lowBound, int highBound);

    ValueInst * fVectorSize;
};

#endif
