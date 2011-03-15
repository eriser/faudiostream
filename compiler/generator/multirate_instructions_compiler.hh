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
#include "sigrateinference.hh"
#include "sigtyperules.hh"
#include "loki/SafeFormat.h"

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
    ValueInst * compileScalarBinop(Tree sig, int opcode, Tree arg1, Tree arg2, FIRIndex const & index);
    ValueInst * compileVectorBinop(Tree sig, int opcode, Tree arg1, Tree arg2, FIRIndex const & index);

    StatementInst * compileAssignmentVectorize(Address * vec, Tree sig, FIRIndex const & index, Tree arg1, Tree arg2);
    StatementInst * compileAssignmentSerialize(Address * vec, Tree sig, FIRIndex const & index, Tree arg1);
    StatementInst * compileAssignmentConcat(Address * vec, Tree sig, FIRIndex const & index, Tree arg1, Tree arg2);
    StatementInst * compileAssignmentAt(Address * vec, Tree sig, FIRIndex const & index, Tree arg1, Tree arg2);
    StatementInst * compileAssignmentProjection(Address * vec, Tree sig, FIRIndex const & index, int i, Tree arg);

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
    Typed * declareSignalType(Typed * type);
    ArrayTyped * declareArrayTyped(Typed * typed, int size);

    ForLoopInst* genSubloop(string const & loopSymbol, int lowBound, int highBound);

    ValueInst * fVectorSize;


    template <typename ArgumentIterator,
              class compilePrimitiveFunctor
             >
    ValueInst * compileVectorSample(Tree sig, ArgumentIterator argsBegin, ArgumentIterator argsEnd,
                                    FIRIndex const & index, compilePrimitiveFunctor const & generatePrimitive)
    {
        const int sigRate = getSigRate(sig);
        Typed * resultTyped = declareSignalType(sig);
        const size_t argumentCount = argsEnd - argsBegin;

        vector<ValueInst*> args;
        vector<Typed*> argTypes;
        vector<int> argDimensions;
        for (ArgumentIterator it = argsBegin; it != argsEnd; ++it) {
            args.push_back(compileSample(*it, index));
            Typed * argType = declareSignalType(*it);
            argTypes.push_back(argType);
            argDimensions.push_back(argType->dimension());
        }

        const int largestArgument = std::max_element(argDimensions.begin(), argDimensions.end()) - argDimensions.begin();
        const int maxDimension = argDimensions[largestArgument];
        assert (maxDimension > 0);

        vector<int> dimensions = dynamic_cast<ArrayTyped*>(argTypes[largestArgument])->dimensions();

        vector<ValueInst*> loopIndexStack;
        ForLoopInst * loopTop = NULL;

        string primitiveId = getFreshID("primitive_");

        vector<string> scalarNames; vector<ValueInst*> scalarArguments(argumentCount, NULL);
        for (size_t i = 0; i != argumentCount; ++i) {
            string name;
            Loki::SPrintf(name, "%s_%d")(primitiveId)(i);
            scalarNames.push_back(name);
        }

        Typed * resultBufferType = declareSignalType(InstBuilder::genArrayTyped(resultTyped, sigRate * gVecSize));
        BasicTyped* resultBasicType = InstBuilder::genBasicTyped(resultBufferType->getVarType());

        DeclareVarInst* resultBuffer = InstBuilder::genDecStackVar(primitiveId + "_result", resultBufferType);
        pushDeclare(resultBuffer);

        int currentDimension = maxDimension;
        do {
            string loopVar;
            Loki::SPrintf(loopVar, "%s_index_%d")(primitiveId)(currentDimension);

            ForLoopInst * loop = genSubloop(loopVar, 0, dimensions[currentDimension-1]);
            loopIndexStack.push_back(loop->loadDeclaration());

            // declare scalar as local variable
            for (size_t i = 0; i != args.size(); ++i) {
                if (argDimensions[i] == currentDimension) {
                    ValueInst * compiledExpression = args[i];
                    LoadVarInst* loadCompiledExpression = dynamic_cast<LoadVarInst*>(compiledExpression);
                    assert(loadCompiledExpression);

                    vector <ValueInst*> loadIndex = loopIndexStack;
                    loadIndex.push_back(getCurrentLoopIndex()); // we need to prefix the index with the current loop index
                    ValueInst * argExpression = InstBuilder::genLoadArrayStructVar(loadCompiledExpression->fAddress->getName(),
                                                                                loadIndex.begin(), loadIndex.end());

                    if (argTypes[i]->getVarType() != resultBasicType->getVarType())
                        argExpression = InstBuilder::genCastNumInst(argExpression, resultBasicType);

                    DeclareVarInst* scalarDeclaration = InstBuilder::genDecStackVar(scalarNames[i], resultBasicType);
                    loop->pushBackInst(scalarDeclaration);
                    loop->pushBackInst(scalarDeclaration->store(argExpression));
                    scalarArguments[i] = scalarDeclaration->load();
                }
            }

            if (loopTop == 0)
                pushComputeDSPMethod(loop);
            else
                loopTop->fCode->pushBackInst(loop);
            loopTop = loop;
        } while (currentDimension-- > 1);

        // collect remaining arguments
        for (size_t i = 0; i != args.size(); ++i)
            if (argDimensions[i] == 0) {
                if (argTypes[i]->getVarType() != resultBasicType->getType())
                    scalarArguments[i] = InstBuilder::genCastNumInst(args[i], resultBasicType);
                else
                    scalarArguments[i] = args[i];
            }

        ValueInst * scalarValue = generatePrimitive(scalarArguments.begin(), scalarArguments.end());

        vector <ValueInst*> storeIndex = loopIndexStack;
        storeIndex.push_back(getCurrentLoopIndex()); // we need to prefix the index with the current loop index
        StoreVarInst * store = InstBuilder::genStoreArrayStructVar(resultBuffer->getName(), resultBufferType, scalarValue,
                                                                storeIndex.begin(), storeIndex.end());
        loopTop->pushBackInst(store);

        ValueInst * result = InstBuilder::genLoadVarInst(InstBuilder::genIndexedAddress(resultBuffer->fAddress,
                                                                                        getCurrentLoopIndex()));

        return result;
    }
};

#endif
