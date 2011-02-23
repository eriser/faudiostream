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

#include <limits>

#include "multirate_instructions_compiler.hh"
#include "sigrateinference.hh"
#include "sigtyperules.hh"
#include "loki/SafeFormat.h"


void MultirateInstructionsCompiler::compileMultiSignal(Tree L)
{
    L = prepare(L);     // Optimize, share and annotate expression

    Typed* type;
    /*
    if (gVectorSwitch) {
        type =  InstBuilder::genArrayTyped(InstBuilder::genVectorTyped(InstBuilder::genBasicTyped(Typed::kFloatMacro)), 0);
    } else {
        type = InstBuilder::genArrayTyped(InstBuilder::genBasicTyped(Typed::kFloatMacro), 0);
    }
    */
    type = InstBuilder::genArrayTyped(InstBuilder::genBasicTyped(Typed::kFloatMacro), 0);

    for (int index = 0; index < fContainer->inputs(); index++) {
        string name1 = subst("fInput$0_ptr", T(index));
        string name2 = subst("fInput$0", T(index));
        pushDeclare(InstBuilder::genDecStructVar(name1, type));
        pushComputeBlockMethod(InstBuilder::genStoreStructVar(name1,
            InstBuilder::genLoadArrayFunArgsVar("inputs", InstBuilder::genIntNumInst(index))));
        pushDeclare(InstBuilder::genDecStructVar(name2, type));
    }

    // "output" and "outputs" used as a name convention
    for (int index = 0; index < fContainer->outputs(); index++) {
        string name1 = subst("fOutput$0_ptr", T(index));
        string name2 = subst("fOutput$0", T(index));
        pushDeclare(InstBuilder::genDecStructVar(name1, type));
        pushComputeBlockMethod(InstBuilder::genStoreStructVar(name1,
            InstBuilder::genLoadArrayFunArgsVar("outputs", InstBuilder::genIntNumInst(index))));
        pushDeclare(InstBuilder::genDecStructVar(name2, type));
    }


    compileTop(L);

    generateUserInterfaceTree(prepareUserInterfaceTree(fUIRoot));
    generateMacroInterfaceTree("", prepareUserInterfaceTree(fUIRoot));
    if (fDescription)
        fDescription->ui(prepareUserInterfaceTree(fUIRoot));

    fContainer->processFIR();
}

DeclareVarInst * declareIntVarOnStack(string const & name, ValueInst * val)
{
    return InstBuilder::genDeclareVarInst(InstBuilder::genNamedAddress(name,
                                                                       Address::kStack),
                                          InstBuilder::genBasicTyped(Typed::kInt),
                                          val);
}

void MultirateInstructionsCompiler::compileTop(Tree rootSignal)
{
    fVectorSize = InstBuilder::genIntNumInst(gVecSize);

    int outputCount = len(rootSignal);

    for (int i = 0; i != outputCount; ++i) {
        Tree sig = nth(rootSignal, i);
        string outputName;
        Loki::SPrintf(outputName, "output%d")(i);

        VectorAddress * outI = new VectorAddress(outputName, InstBuilder::genBasicTyped(Typed::kFloat),
                                                 numeric_limits<int>::max(),  // FIXME: pessimise, we cannot know the `count' value at compile-time
                                                 Address::kFunArgs);          // FIXME: what is the access type?
        compileVector(outI, sig);
    }
}


void MultirateInstructionsCompiler::compileVector(VectorAddress * vec, Tree sig)
{
    int sigRate = getSigRate(sig);

    ValueInst * rate = InstBuilder::genIntNumInst(sigRate);

    DeclareVarInst* loop_decl = InstBuilder::genDecLoopVar("j", InstBuilder::genBasicTyped(Typed::kInt), InstBuilder::genIntNumInst(0));
    ValueInst* loop_end = InstBuilder::genLessThan(loop_decl->load(), InstBuilder::genMul(rate, fVectorSize));
    StoreVarInst* loop_increment = loop_decl->store(InstBuilder::genAdd(loop_decl->load(), 1));
    ForLoopInst* loop = InstBuilder::genForLoopInst(loop_decl, loop_end, loop_increment);

    loop->pushFrontInst(compileAssignment(vec, sig, loop_decl->load()));

    pushComputeDSPMethod(loop);
}

StatementInst * MultirateInstructionsCompiler::compileAssignment(Address * vec, Tree sig, ValueInst * index)
{
    IndexedAddress * dest = InstBuilder::genIndexedAddress(vec, index);

    int i;
    if (isSigInt(sig, &i))
        return store(dest, compileSample(sig, index));

    double r;
    if (isSigReal(sig, &r))
        return store(dest, compileSample(sig, index));

    if (isSigInput(sig, &i))
        return store(dest, compileSample(sig, index));

    throw std::runtime_error("not implemented");
    return NULL;
}

ValueInst * MultirateInstructionsCompiler::compileSample(Tree sig, ValueInst * index)
{
    int i;
    if (isSigInt(sig, &i))
        return InstBuilder::genIntNumInst(i);

    double r;
    if (isSigReal(sig, &r))
        return InstBuilder::genRealNumInst(itfloat(), r);

    if (isSigInput(sig, &i))
        return compileSampleInput(sig, i, index);

    throw std::runtime_error("not implemented");
    return NULL;
}

ValueInst * MultirateInstructionsCompiler::compileSampleInput(Tree sig, int i, ValueInst * index)
{
    string name = subst("input$0", T(i));
    LoadVarInst * res = InstBuilder::genLoadArrayStackVar(name, index);

    ValueInst * castedToFloat = InstBuilder::genCastNumInst(res, InstBuilder::genBasicTyped(itfloat()));

    return castedToFloat;
}


StatementInst * MultirateInstructionsCompiler::store (Address * address, ValueInst * value)
{
    return InstBuilder::genStoreVarInst(address, value);
}
