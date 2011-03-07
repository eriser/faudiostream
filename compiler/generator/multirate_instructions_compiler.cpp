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
    pushGlobalDeclare(InstBuilder::genDeclareTypeInst(type));

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
        int rate = getSigRate(sig);
        fContainer->setOutputRate(i, rate);

        string outputName;
        Loki::SPrintf(outputName, "fOutput%d")(i);

        NamedAddress * outI = InstBuilder::genVectorAddress(outputName, InstBuilder::genBasicTyped(Typed::kFloat),
                                                            0, // arbitrary size
                                                            Address::kFunArgs); // FIXME: what is the access type?
        compileVector(outI, sig);
    }
}


void MultirateInstructionsCompiler::compileVector(NamedAddress * vec, Tree sig)
{
    int sigRate = getSigRate(sig);

    ValueInst * rate = InstBuilder::genIntNumInst(sigRate);

    DeclareVarInst* loop_decl = InstBuilder::genDecLoopVar("j", InstBuilder::genBasicTyped(Typed::kInt), InstBuilder::genIntNumInst(0));
    ValueInst* loop_end = InstBuilder::genLessThan(loop_decl->load(), rate);
    StoreVarInst* loop_increment = loop_decl->store(InstBuilder::genAdd(loop_decl->load(), 1));
    ForLoopInst* loop = InstBuilder::genForLoopInst(loop_decl, loop_end, loop_increment);

    FIRIndex index = FIRIndex(getCurrentLoopIndex()) * rate + loop_decl->load();

    IndexedAddress * storeAddress = InstBuilder::genIndexedAddress(vec, index);

    loop->pushFrontInst(compileAssignment(storeAddress, sig, index));

    pushComputeDSPMethod(loop);
}

static bool isPrimitive(Tree sig)
{
    int     i;
    double  r;
    Tree    c, sel, x, y, z, label, id, ff, largs, type, name, file;

    if (   getUserData(sig)
        || isSigBinOp(sig, &i, x, y)
        || isSigFFun(sig, ff, largs)
        || isSigFConst(sig, type, name, file)
        || isSigFVar(sig, type, name, file)

        || isSigTable(sig, id, x, y)
        || isSigWRTbl(sig, id, x, y, z)
        || isSigRDTbl(sig, x, y)

        || isSigSelect2(sig, sel, x, y)
        || isSigSelect3(sig, sel, x, y, z)

        || isSigGen(sig, x)

        || isSigIntCast(sig, x)
        || isSigFloatCast(sig, x)

        || isSigButton(sig, label)
        || isSigCheckbox(sig, label)
        || isSigVSlider(sig, label,c,x,y,z)
        || isSigHSlider(sig, label,c,x,y,z)
        || isSigNumEntry(sig, label,c,x,y,z)

        || isSigVBargraph(sig, label,x,y,z)
        || isSigHBargraph(sig, label,x,y,z)
        || isSigAttach(sig, x, y) )
        return true;
    else
        return false;
}

StatementInst * MultirateInstructionsCompiler::compileAssignment(Address * dest, Tree sig, FIRIndex const & index)
{
    int i;
    if (isSigInt(sig, &i))
        return store(dest, compileSample(sig, index));

    double r;
    if (isSigReal(sig, &r))
        return store(dest, compileSample(sig, index));

    if (isSigInput(sig, &i))
        return store(dest, compileSample(sig, index));

    Tree arg1, arg2;
    if (isSigVectorize(sig, arg1, arg2))
        return compileAssignmentVectorize(dest, sig, index, arg1, arg2);

    if (isSigSerialize(sig, arg1))
        return compileAssignmentSerialize(dest, sig, index, arg1);

    if (isSigConcat(sig, arg1, arg2))
        return compileAssignmentConcat(dest, sig, index, arg1, arg2);

    if (isSigVectorAt(sig, arg1, arg2))
        return compileAssignmentAt(dest, sig, index, arg1, arg2);

    if (isPrimitive(sig))
        return store(dest, compileSample(sig, index));

    throw std::runtime_error("not implemented");
    return NULL;
}

ValueInst * MultirateInstructionsCompiler::compileSample(Tree sig, FIRIndex const & index)
{
    int i;
    if (isSigInt(sig, &i))
        return InstBuilder::genIntNumInst(i);

    double r;
    if (isSigReal(sig, &r))
        return InstBuilder::genRealNumInst(itfloat(), r);

    if (isSigInput(sig, &i))
        return compileSampleInput(sig, i, index);

    Tree arg1, arg2;
    if (isSigVectorize(sig, arg1, arg2))
        return compileSampleVectorize(sig, index, arg1, arg2);

    if (isSigSerialize(sig, arg1))
        return compileSampleSerialize(sig, index, arg1);

    if (isSigConcat(sig, arg1, arg2))
        return compileSampleConcat(sig, index, arg1, arg2);

    if (isSigVectorAt(sig, arg1, arg2))
        return compileSampleAt(sig, index, arg1, arg2);

    if (isPrimitive(sig))
        return compileSamplePrimitive(sig, index);

    throw std::runtime_error("not implemented");
    return NULL;
}

ValueInst * MultirateInstructionsCompiler::compileSampleInput(Tree sig, int i, FIRIndex const & index)
{
    int rate = getSigRate(sig);
    fContainer->setInputRate(i, rate);

    string name = subst("fInput$0", T(i));
    LoadVarInst * res = InstBuilder::genLoadArrayStackVar(name, index);

    ValueInst * castedToFloat = InstBuilder::genCastNumInst(res, InstBuilder::genBasicTyped(itfloat()));
    return castedToFloat;
}


ValueInst * MultirateInstructionsCompiler::compileSamplePrimitive(Tree sig, FIRIndex const & index)
{
    if (isShared(sig)) {
        ValueInst * compiledInstruction;
        DeclareVarInst * cachedValue;
        if (getCompiledExpression(sig, compiledInstruction)) {
            cachedValue = dynamic_cast<DeclareVarInst*>(compiledInstruction);
        } else {
            int rate = getSigRate(sig);
            AudioType* type = getSigType(sig);
            DeclareTypeInst* sampleTypeDeclaration = InstBuilder::genType(type);
            ArrayTyped* sampleArrayType = InstBuilder::genArrayTyped(sampleTypeDeclaration->fType,
                                                                     rate * gVecSize);

            pushGlobalDeclare(sampleTypeDeclaration);
            pushGlobalDeclare(InstBuilder::genDeclareTypeInst(sampleArrayType));

            cachedValue = InstBuilder::genDecStackVar(getFreshID("cacheVector"), sampleArrayType);

            fContainer->openLoop(getFreshID("i_"), rate);
            IndexedAddress * storeAddress = InstBuilder::genIndexedAddress(cachedValue->getAddress(), getCurrentLoopIndex());
            pushComputeDSPMethod(store(storeAddress, compilePrimitive(sig, index)));
            fContainer->closeLoop();
        }

        IndexedAddress * addressToReturn = InstBuilder::genIndexedAddress(cachedValue->getAddress(), index);
        return InstBuilder::genLoadVarInst(addressToReturn);

    } else {
        return compilePrimitive(sig, index);
    }
}

ValueInst * MultirateInstructionsCompiler::compileBinop(Tree sig, int opcode, Tree arg1, Tree arg2, FIRIndex const & index)
{
    int t1 = getSigType(arg1)->nature();
    int t2 = getSigType(arg2)->nature();
    int t3 = getSigType(sig)->nature();

    ValueInst* res = NULL;
    ValueInst* val1 = compileSample(arg1, index);
    ValueInst* val2 = compileSample(arg2, index);

    // Arguments and expected result type analysis, add the required "cast" when needed
    if (t1 == kReal) {
        if (t2 == kReal) {
            res = InstBuilder::genBinopInst(opcode, val1, val2);
            if (t3 == kReal) {
                // Nothing
            } else {
                res = InstBuilder::genCastNumInst(res, InstBuilder::genBasicTyped(Typed::kInt));
            }
        } else {
            res = InstBuilder::genBinopInst(opcode, val1, InstBuilder::genCastNumInst(val2, InstBuilder::genBasicTyped(itfloat())));
            if (t3 == kReal) {
                // Nothing
            } else {
                res = InstBuilder::genCastNumInst(res, InstBuilder::genBasicTyped(Typed::kInt));
            }
        }
    } else if (t2 == kReal) {
        res = InstBuilder::genBinopInst(opcode, InstBuilder::genCastNumInst(val1, InstBuilder::genBasicTyped(itfloat())), val2);
        if (t3 == kReal) {
            // Nothing
        } else {
            res = InstBuilder::genCastNumInst(res, InstBuilder::genBasicTyped(Typed::kInt));
        }
    } else {
        res = InstBuilder::genBinopInst(opcode, val1, val2);
        if (t3 == kReal) {
            res = InstBuilder::genCastNumInst(res, InstBuilder::genBasicTyped(itfloat()));
        } else {
            // Nothing
        }
    }

    return res;
}

ValueInst * MultirateInstructionsCompiler::compilePrimitive(Tree sig, FIRIndex const & index)
{
    int     i;
    double  r;
    Tree    c, sel, x, y, z, label, id, ff, largs, type, name, file;

    if (isSigBinOp(sig, &i, x, y))
        return compileBinop(sig, i, x, y, index);


    throw std::runtime_error("not implemented");
}


StatementInst * MultirateInstructionsCompiler::compileAssignmentVectorize(Address * vec, Tree sig,
                                                                          FIRIndex const & index, Tree arg1, Tree arg2)
{
    if (!isShared(sig)) {
        IntNumInst * n = InstBuilder::genIntNumInst(tree2int(arg2));

        DeclareVarInst* loop_decl = InstBuilder::genDecLoopVar(getFreshID("k"), InstBuilder::genBasicTyped(Typed::kInt),
                                                               InstBuilder::genIntNumInst(0));
        ValueInst* loop_end = InstBuilder::genLessThan(loop_decl->load(), n);
        StoreVarInst* loop_increment = loop_decl->store(InstBuilder::genAdd(loop_decl->load(), 1));
        ForLoopInst* loop = InstBuilder::genForLoopInst(loop_decl, loop_end, loop_increment);

        IndexedAddress * destination = InstBuilder::genIndexedAddress(vec, loop_decl->load());
        FIRIndex computeIndex = index * n + loop_decl->load();

        StatementInst * blockInst = compileAssignment(destination, arg1, computeIndex);
        loop->pushBackInst(blockInst);
        return loop;
    } else {
        return store(vec, compileSample(sig, index));
    }
}

ValueInst * MultirateInstructionsCompiler::compileSampleVectorize(Tree sig, FIRIndex const & index, Tree arg1, Tree arg2)
{
    ValueInst * n = InstBuilder::genIntNumInst(tree2int(arg2));

    int sigRate = getSigRate(sig);
    AudioType * sigType = getSigType(sig);
    DeclareTypeInst * declareSigType = InstBuilder::genType(sigType);
    pushGlobalDeclare(declareSigType);
    Typed * sigTyped = declareSigType->fType;

    ArrayTyped * resultBufferType = InstBuilder::genArrayTyped(sigTyped, sigRate * gVecSize);
    DeclareTypeInst * declareResultBufferType = InstBuilder::genDeclareTypeInst(resultBufferType);
    pushGlobalDeclare(declareResultBufferType);

    DeclareVarInst * declareResultBuffer = InstBuilder::genDecStackVar(getFreshID("W"), declareResultBufferType->fType);

    pushDeclare(declareResultBuffer);

    fContainer->openLoop(getFreshID("j_"), sigRate);

    DeclareVarInst* loop_decl = InstBuilder::genDecLoopVar(getFreshID("k"), InstBuilder::genBasicTyped(Typed::kInt),
                                                        InstBuilder::genIntNumInst(0));
    ValueInst* loop_end = InstBuilder::genLessThan(loop_decl->load(), n);
    StoreVarInst* loop_increment = loop_decl->store(InstBuilder::genAdd(loop_decl->load(), 1));
    ForLoopInst* loop = InstBuilder::genForLoopInst(loop_decl, loop_end, loop_increment);

    NamedAddress * resultBufferAddress = InstBuilder::genVectorAddress(declareResultBuffer->getName(),
                                                                        resultBufferType->fType,
                                                                        resultBufferType->fSize,
                                                                        Address::kStack);

    IndexedAddress * compileAddress = InstBuilder::genIndexedAddress(InstBuilder::genIndexedAddress(resultBufferAddress,
                                                                                                    getCurrentLoopIndex()),
                                                                     loop_decl->load());

    FIRIndex compileIndex = FIRIndex(getCurrentLoopIndex()) * n + loop_decl->load();

    loop->pushBackInst(compileAssignment(compileAddress, arg1, compileIndex));

    pushComputeDSPMethod(loop);
    fContainer->closeLoop();

    IndexedAddress * addressToReturn = InstBuilder::genIndexedAddress(declareResultBuffer->getAddress(), index);
    return InstBuilder::genLoadVarInst(addressToReturn);
}

StatementInst * MultirateInstructionsCompiler::compileAssignmentSerialize(Address * vec, Tree sig,
                                                                          FIRIndex const & index, Tree arg1)
{
    if (!isShared(sig)) {
        int m = getSigRate(arg1);
        int n = getSigRate(sig) / getSigRate(arg1);

        DeclareTypeInst* declareArgType = InstBuilder::genType(getSigType(arg1));
        pushGlobalDeclare(declareArgType);

        CastAddress * castedResultAddress = InstBuilder::genCastAddress(vec, declareArgType->fType);

        ValueInst * condition = InstBuilder::genBinopInst(kEQ,
                                                        InstBuilder::genBinopInst(kRem, index, InstBuilder::genIntNumInst(n)),
                                                        InstBuilder::genIntNumInst(0));

        FIRIndex indexInSource = index / n;
        BlockInst * thenCase = InstBuilder::genBlockInst();
        thenCase->pushBackInst(compileAssignment(castedResultAddress, arg1, indexInSource));
        return InstBuilder::genIfInst(condition, thenCase);
    } else
        return store(vec, compileSampleSerialize(sig, index, arg1));
}

ValueInst * MultirateInstructionsCompiler::compileSampleSerialize(Tree sig, FIRIndex const & index, Tree arg1)
{
    DeclareTypeInst* declareSigType = InstBuilder::genType(getSigType(sig));
    DeclareTypeInst* declareArgType = InstBuilder::genType(getSigType(arg1));
    pushGlobalDeclare(declareSigType);
    pushGlobalDeclare(declareArgType);

    int m = getSigRate(arg1);
    int n = getSigRate(sig) / getSigRate(arg1);

    int loopSize = n * m;

    ArrayTyped* resultBufferType = InstBuilder::genArrayTyped(declareSigType->fType, loopSize);
    pushGlobalDeclare(InstBuilder::genDeclareTypeInst(resultBufferType));

    DeclareVarInst * declareResultBuffer = InstBuilder::genDecStackVar(getFreshID("W"), resultBufferType);

    pushDeclare(declareResultBuffer);

    fContainer->openLoop(getFreshID("j_"), loopSize);

    ValueInst * condition = InstBuilder::genBinopInst(kEQ,
                                                      InstBuilder::genBinopInst(kRem, getCurrentLoopIndex(), InstBuilder::genIntNumInst(n)),
                                                      InstBuilder::genIntNumInst(0));


    NamedAddress * resultBufferAddress = InstBuilder::genVectorAddress(declareResultBuffer->getName(),
                                                                        resultBufferType->fType,
                                                                        resultBufferType->fSize,
                                                                        Address::kStack);

    IndexedAddress * destinationAddress = InstBuilder::genIndexedAddress(resultBufferAddress,
                                                                         getCurrentLoopIndex());
    CastAddress * castedDestiationAddress = InstBuilder::genCastAddress(destinationAddress, declareArgType->fType);

    FIRIndex compileIndex = FIRIndex(getCurrentLoopIndex()) / n;
    BlockInst * thenCase = InstBuilder::genBlockInst();
    thenCase->pushBackInst(compileAssignment(castedDestiationAddress, arg1, compileIndex));

    StatementInst* ifStatement = InstBuilder::genIfInst(condition, thenCase);

    pushComputeDSPMethod(ifStatement);

    fContainer->closeLoop();

    IndexedAddress * addressToReturn = InstBuilder::genIndexedAddress(declareResultBuffer->getAddress(), index);
    return InstBuilder::genLoadVarInst(addressToReturn);
}

ValueInst * MultirateInstructionsCompiler::compileSampleConcat(Tree sig, FIRIndex const & index, Tree arg1, Tree arg2)
{
    int rate = getSigRate(sig);
    assert(getSigRate(sig) == getSigRate(arg1) && getSigRate(sig) == getSigRate(arg2));

    AudioType * resultType = getSigType(sig);
    AudioType * argType1 = getSigType(arg1);
    AudioType * argType2 = getSigType(arg2);

    FaustVectorType * vResultType = isVectorType(resultType);
    FaustVectorType * vArgType1 = isVectorType(argType1);
    FaustVectorType * vArgType2 = isVectorType(argType2);
    assert(vResultType && vArgType1 && vArgType2);

    DeclareTypeInst* declareSigType = InstBuilder::genType(resultType);
    pushGlobalDeclare(declareSigType);

    DeclareTypeInst* declareArgType1 = InstBuilder::genType(argType1);
    pushGlobalDeclare(declareArgType1);

    DeclareTypeInst* declareArgType2 = InstBuilder::genType(argType2);
    pushGlobalDeclare(declareArgType2);

    ArrayTyped* resultBufferType = InstBuilder::genArrayTyped(declareSigType->fType, rate * gVecSize);
    pushGlobalDeclare(InstBuilder::genDeclareTypeInst(resultBufferType));

    DeclareVarInst * declareResultBuffer = InstBuilder::genDecStackVar(getFreshID("W"), resultBufferType);

    fContainer->openLoop(getFreshID("j_"), rate);

    IndexedAddress * resultAddress1 = InstBuilder::genIndexedAddress(InstBuilder::genIndexedAddress(declareResultBuffer->getAddress(),
                                                                                                    getCurrentLoopIndex()),
                                                                     InstBuilder::genIntNumInst(0));
    IndexedAddress * resultAddress2 = InstBuilder::genIndexedAddress(InstBuilder::genIndexedAddress(declareResultBuffer->getAddress(),
                                                                                                    getCurrentLoopIndex()),
                                                                     InstBuilder::genIntNumInst(vArgType1->size()));

    CastAddress * castedResultAddress1 = InstBuilder::genCastAddress(resultAddress1, declareArgType1->fType);
    CastAddress * castedResultAddress2 = InstBuilder::genCastAddress(resultAddress2, declareArgType2->fType);

    compileAssignment(castedResultAddress1, arg1, FIRIndex(getCurrentLoopIndex()));
    compileAssignment(castedResultAddress2, arg2, FIRIndex(getCurrentLoopIndex()));

    fContainer->closeLoop();

    IndexedAddress * addressToReturn = InstBuilder::genIndexedAddress(declareResultBuffer->getAddress(), index);
    return InstBuilder::genLoadVarInst(addressToReturn);
}

StatementInst * MultirateInstructionsCompiler::compileAssignmentConcat(Address * vec, Tree sig, FIRIndex const & index, Tree arg1, Tree arg2)
{
    if (!isShared(sig)) {
        assert(getSigRate(sig) == getSigRate(arg1) && getSigRate(sig) == getSigRate(arg2));

        AudioType * resultType = getSigType(sig);
        AudioType * argType1 = getSigType(arg1);
        AudioType * argType2 = getSigType(arg2);

        FaustVectorType * vResultType = isVectorType(resultType);
        FaustVectorType * vArgType1 = isVectorType(argType1);
        FaustVectorType * vArgType2 = isVectorType(argType2);
        assert(vResultType && vArgType1 && vArgType2);

        DeclareTypeInst* declareSigType = InstBuilder::genType(resultType);
        pushGlobalDeclare(declareSigType);

        DeclareTypeInst* declareArgType1 = InstBuilder::genType(argType1);
        pushGlobalDeclare(declareArgType1);

        DeclareTypeInst* declareArgType2 = InstBuilder::genType(argType2);
        pushGlobalDeclare(declareArgType2);

        IndexedAddress * resultAddress1 = InstBuilder::genIndexedAddress(vec, InstBuilder::genIntNumInst(0));
        IndexedAddress * resultAddress2 = InstBuilder::genIndexedAddress(vec, InstBuilder::genIntNumInst(vArgType1->size()));

        CastAddress * castedResultAddress1 = InstBuilder::genCastAddress(resultAddress1, declareArgType1->fType);
        CastAddress * castedResultAddress2 = InstBuilder::genCastAddress(resultAddress2, declareArgType2->fType);

        StatementInst* ca1 = compileAssignment(castedResultAddress1, arg1, index);
        StatementInst* ca2 = compileAssignment(castedResultAddress2, arg2, index);
        BlockInst * returnBlock = InstBuilder::genBlockInst();
        returnBlock->pushBackInst(ca1);
        returnBlock->pushBackInst(ca2);
        return returnBlock;
    } else {
        return store(vec, compileSampleConcat(sig, index, arg1, arg2));
    }
}


StatementInst * MultirateInstructionsCompiler::compileAssignmentAt(Address * vec, Tree sig, FIRIndex const & index, Tree arg1, Tree arg2)
{
    return store(vec, compileSampleAt(sig, index, arg1, arg2));
}

ValueInst * MultirateInstructionsCompiler::compileSampleAt(Tree sig, FIRIndex const & index, Tree arg1, Tree arg2)
{
    assert(isVectorType(getSigType(arg1)));

    ValueInst * compiledArg1 = compileSample(arg1, index);
    ValueInst * compiledArg2 = compileSample(arg2, index);

    LoadVarInst * loadArg1 = dynamic_cast<LoadVarInst*>(compiledArg1);

    IndexedAddress * addressToLoad  = InstBuilder::genIndexedAddress(loadArg1->fAddress, compiledArg2);

    return InstBuilder::genLoadVarInst(addressToLoad);
}


StatementInst * MultirateInstructionsCompiler::store (Address * address, ValueInst * value)
{
    return InstBuilder::genStoreVarInst(address, value);
}
