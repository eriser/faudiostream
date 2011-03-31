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
    // compiler entry points
    void compileMultiSignal(Tree rootSignal);
    void compileSingleSignal(Tree rootSignal);

    // basic compilation schemes:
    void compileRecursiveGroups(Tree rootSignal);     // compile recursive groups from the leaves to the root
    void compileTop(Tree rootSignal);                 // compile the non-recursive signal from the root
    void compileVector(NamedAddress * vec, Tree sig); // compile a signal vector
    StatementInst * compileAssignment(Address * vec, Tree sig, FIRIndex const & index); // compile assignment to address `vec' of signal `sig' at index `index'
    ValueInst * compileSample(Tree sig, FIRIndex const & index); // compile sample of signal `sig' at index `index'

    // signal-specific compilation rules
    ValueInst * compileSampleInput(Tree sig, int i, FIRIndex const & index);
    ValueInst * compileSampleVectorize(Tree sig, FIRIndex const & index, Tree arg1, Tree arg2);
    ValueInst * compileSampleSerialize(Tree sig, FIRIndex const & index, Tree arg1);
    ValueInst * compileSampleConcat(Tree sig, FIRIndex const & index, Tree arg1, Tree arg2);
    ValueInst * compileSampleAt(Tree sig, FIRIndex const & index, Tree arg1, Tree arg2);
    ValueInst * compileSamplePrimitive(Tree sig, FIRIndex const & index);
    ValueInst * compilePrimitive(Tree sig, FIRIndex const & index);
    ValueInst * compileSampleDelay(Tree sig, FIRIndex const & index, Tree delayline, Tree delay);

    ValueInst * compileBinop(Tree sig, int opcode, Tree arg1, Tree arg2, FIRIndex const & index);
    ValueInst * compileXtended(Tree sig, FIRIndex const & index);
    ValueInst * compileFFun(Tree sig, Tree ff, Tree args, FIRIndex const & index);
    ValueInst * loadForeignVar(Tree sig, Tree type, Tree name, Tree file, FIRIndex const & index);
    ValueInst * compileCast(Tree sig, Tree arg, Typed::VarType type, FIRIndex const & index);
    ValueInst * compileSelect2(Tree sig, Tree selector, Tree x, Tree y, FIRIndex const & index);
    ValueInst * compileSelect3(Tree sig, Tree selector, Tree x, Tree y, Tree z, FIRIndex const & index);

    ValueInst * compileButton(Tree sig, Tree path, const string & name, FIRIndex const & index);
    ValueInst * compileSlider(Tree sig, Tree path, Tree cur, Tree min, Tree max, Tree step, const string& name, FIRIndex const & index);
    ValueInst * compileBargraph(Tree sig, Tree path, Tree min, Tree max, Tree value, const string& name, FIRIndex const & index);

    ValueInst * compileSampleRDTable(Tree sig, FIRIndex const & index, Tree table, Tree tableIndex);
    ValueInst * compileSampleWRTable(Tree sig, FIRIndex const & index, Tree table, Tree writeIndex, Tree writeStream);
    NamedAddress * generateTable(Tree table, Tree tableID, Tree tableSize, Tree tableInitializationSignal, bool canBeShared);
    CodeContainer* signal2Container(const string& name, Tree sig);

    StatementInst * compileAssignmentVectorize(Address * vec, Tree sig, FIRIndex const & index, Tree arg1, Tree arg2);
    StatementInst * compileAssignmentSerialize(Address * vec, Tree sig, FIRIndex const & index, Tree arg1);
    StatementInst * compileAssignmentConcat(Address * vec, Tree sig, FIRIndex const & index, Tree arg1, Tree arg2);
    StatementInst * compileAssignmentAt(Address * vec, Tree sig, FIRIndex const & index, Tree arg1, Tree arg2);
    StatementInst * compileAssignmentProjection(Address * vec, Tree sig, FIRIndex const & index, int i, Tree arg);

    Address * compileDelayline(Tree delayline);
    Address * declareDelayLine(Tree delayline);

    // cache handling
    void setCompiledCache(Tree sig, LoadVarInst * loadCacheInst);
    ValueInst * getCompiledCache(Tree sig, FIRIndex const & index); // implicitly adds graph dependency

    // FIR helper functions: mainly syntactic sugar for InstBuilder
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
    ValueInst * fVectorSize; ///< shortcut for accessing the gVecSize as FIR instruction

    // loop handling helper functions
    void openLoop(Tree recursiveSymbol, int size = 1)
    {
        fContainer->openLoop(recursiveSymbol, "j", size);
    }

    void openLoop(int size = 1)
    {
        fContainer->openLoop("j", size);
    }

    void closeLoop(void)
    {
        fContainer->closeLoop();
    }

    // polymorphic function generators

    /** use functor to compile possibly multidimensional primitives
     *
     * depending on the signal type, it compiles a scalar primitive or manually creates loops to implement multidimensional
     * signals with scalar primitives
     *
     * \param sig signal to compute
     * \param arguments container of all argument signals
     * \param index fir index to compute
     * \param functor requires an overloaded operator, taking an iterator range as arguments. dereferencing the iterator
     * is assumed to return a ValueInst
     * \param generateCasts if `true', arguments are casted to the nature of the signal
     *
     */
    template <typename ArgumentContainer,
              class compilePrimitiveFunctor
             >
    ValueInst * dispatchPolymorphicFunctor(Tree sig, ArgumentContainer const & arguments, FIRIndex const & index,
                                           compilePrimitiveFunctor const & functor, bool generateCasts)
    {
        Typed * resultTyped = declareSignalType(sig);
        if (resultTyped->dimension() == 0)
            return compileScalarSample(sig, arguments.begin(), arguments.end(), index, functor, generateCasts);
        else
            return compileVectorSample(sig, arguments.begin(), arguments.end(), index, functor, generateCasts);
    }

    /* compile multidimensional primitive */
    template <typename ArgumentIterator,
              class compilePrimitiveFunctor
             >
    ValueInst * compileVectorSample(Tree sig, ArgumentIterator argsBegin, ArgumentIterator argsEnd,
                                    FIRIndex const & index, compilePrimitiveFunctor const & generatePrimitive,
                                    bool createCasts = false)
    {
        const int sigRate = getSigRate(sig);
        Typed * resultTyped = declareSignalType(sig);
        const size_t argumentCount = argsEnd - argsBegin;

        openLoop();
        ForLoopInst * rateSubLoop = genSubloop("k", 0, sigRate);
        pushComputeDSPMethod(rateSubLoop);

        vector<ValueInst*> args;
        vector<Typed*> argTypes;
        vector<int> argDimensions;
        // compile all arguments
        for (ArgumentIterator it = argsBegin; it != argsEnd; ++it) {
            args.push_back(compileSample(*it, getCurrentLoopIndex()));
            Typed * argType = declareSignalType(*it);
            argTypes.push_back(argType);
            argDimensions.push_back(argType->dimension());
        }

        const int largestArgument = std::max_element(argDimensions.begin(), argDimensions.end()) - argDimensions.begin();
        const int maxDimension = argDimensions[largestArgument];
        assert (maxDimension > 0);

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

        // create subloops
        vector<int> dimensions = dynamic_cast<ArrayTyped*>(argTypes[largestArgument])->dimensions();
        vector<ValueInst*> loopIndexStack;
        ForLoopInst * loopTop = NULL;
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

                    if (createCasts && argTypes[i]->getVarType() != resultBasicType->getVarType())
                        argExpression = InstBuilder::genCastNumInst(argExpression, resultBasicType);

                    DeclareVarInst* scalarDeclaration = InstBuilder::genDecStackVar(scalarNames[i], resultBasicType);
                    loop->pushBackInst(scalarDeclaration);
                    loop->pushBackInst(scalarDeclaration->store(argExpression));
                    scalarArguments[i] = scalarDeclaration->load();
                }
            }

            if (loopTop == 0)
                rateSubLoop->pushBackInst(loop);
            else
                loopTop->fCode->pushBackInst(loop);
            loopTop = loop;
        } while (currentDimension-- > 1);

        // collect remaining arguments
        for (size_t i = 0; i != args.size(); ++i)
            if (argDimensions[i] == 0) {
                if (createCasts && argTypes[i]->getVarType() != resultBasicType->getType())
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

        closeLoop();

        ValueInst * result = InstBuilder::genLoadVarInst(InstBuilder::genIndexedAddress(resultBuffer->fAddress,
                                                                                        index));

        return result;
    }

    /* compile scalar primitive */
    template <typename ArgumentIterator,
              class compilePrimitiveFunctor
             >
    ValueInst * compileScalarSample(Tree sig, ArgumentIterator argsBegin, ArgumentIterator argsEnd,
                                    FIRIndex const & index, compilePrimitiveFunctor const & generatePrimitive,
                                    bool createCasts = false)
    {
        // FIXME: we are adding explicit cast instructions. however we should re-check if this is required, since some casts are added in the signal domain
        int resultNature = getSigType(sig)->nature();

        vector<int> argumentNatures;
        bool hasFloatArgs = false;
        for (ArgumentIterator it = argsBegin; it != argsEnd; ++it) {
            int nature = getSigType(*it)->nature();
            argumentNatures.push_back(nature);
            if (nature == kReal)
                hasFloatArgs = true;
        }

        // compile arguments
        vector<ValueInst *> scalarArguments;
        for (ArgumentIterator it = argsBegin; it != argsEnd; ++it) {
            Tree arg = *it;
            ValueInst * compiledArg = compileSample(arg, index);
            int argumentNature = getSigType(arg)->nature();

            // under certain conditions, we add explicit type casts
            if (createCasts && hasFloatArgs && (argumentNature == kInt))
                compiledArg = InstBuilder::genCastNumInst(compiledArg, InstBuilder::genBasicTyped(itfloat()));

            scalarArguments.push_back(compiledArg);
        }

        ValueInst * result = generatePrimitive(scalarArguments.begin(), scalarArguments.end());

        // under certain conditions, we add explicit type casts
        if (createCasts && hasFloatArgs && (resultNature == kInt))
            result = InstBuilder::genCastNumInst(result, InstBuilder::genBasicTyped(Typed::kInt));

        return result;
    }
};

#endif
