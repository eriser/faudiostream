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
#include "ensure.hh"

#include "prepare_delaylines.hh"
#include <xtended.hh>
#include <prim2.hh>

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
    type = declareArrayTyped(InstBuilder::genBasicTyped(Typed::kFloatMacro), 0);

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

    ForLoopInst * subloop = genSubloop("j", 0, sigRate);

    FIRIndex index = getCurrentLoopIndex() * sigRate + subloop->loadDeclaration();
    IndexedAddress * storeAddress = InstBuilder::genIndexedAddress(vec, index);
    subloop->pushFrontInst(compileAssignment(storeAddress, sig, index));

    pushComputeDSPMethod(subloop);
}

static bool isPrimitive(Tree sig)
{
    int     i;
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

    if (isSigFixDelay(sig, arg1, arg2))
        return store(dest, compileSample(sig, index));

    if (isProj(sig, &i, arg2))
        return compileAssignmentProjection(dest, sig, index, i, arg2);

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

    if (isSigFixDelay(sig, arg1, arg2))
        return compileSampleDelay(sig, index, arg1, arg2);

    if (isProj(sig, &i, arg2))
        // invariant: projections are followed by delay lines. therefore direct sample computation are not possible
        throw std::logic_error("internal error: compiling sample from projection");


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

            Typed * sampleTyped = declareSignalType(sig);
            ArrayTyped* sampleArrayType = declareArrayTyped(sampleTyped, rate * gVecSize);

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


struct ScalarBinopFunctor
{
    const int fOpcode;

    ScalarBinopFunctor(int opcode):
        fOpcode(opcode)
    {}

    template <typename ArgIterator>
    ValueInst * operator()(ArgIterator argsBegin, ArgIterator argsEnd) const
    {
        const size_t argumentCount = argsEnd - argsBegin;
        assert(argumentCount == 2);
        ValueInst * arg0 = *argsBegin++;
        ValueInst * arg1 = *argsBegin;
        ValueInst * result = InstBuilder::genBinopInst(fOpcode, arg0, arg1);

        return result;
    }
};

ValueInst * MultirateInstructionsCompiler::compileBinop(Tree sig, int opcode, Tree arg1, Tree arg2, FIRIndex const & index)
{
    vector<Tree> arguments;
    arguments.push_back(arg1);
    arguments.push_back(arg2);

    return dispatchPolymorphicFunctor(sig, arguments, index, ScalarBinopFunctor(opcode), true);
}

struct ScalarXtendedFunctor
{
    xtended * fXtended;
    CodeContainer * fContainer;
    vector< ::Type> fScalarArgTypes;
    ::Type fScalarResultType;

    ScalarXtendedFunctor(xtended * xtdd, CodeContainer * container, AudioType * resultType, vector<AudioType*> const & argTypes):
        fXtended(xtdd), fContainer(container)
    {
        // the extended API requires the base audio type, so compute it here.
        fScalarResultType = resultType->getScalarBaseType();
        for (vector<AudioType*>::const_iterator it = argTypes.begin(); it != argTypes.end(); ++it)
            fScalarArgTypes.push_back((*it)->getScalarBaseType());
    }

    template <typename ArgIterator>
    ValueInst * operator()(ArgIterator argsBegin, ArgIterator argsEnd) const
    {
        list<ValueInst*> args(argsBegin, argsEnd);
        const size_t argumentCount = argsEnd - argsBegin;
        assert(argumentCount == fXtended->arity());

        ValueInst * result = fXtended->generateCode(fContainer, args, fScalarResultType, fScalarArgTypes);
        return result;
    }
};



ValueInst * MultirateInstructionsCompiler::compileXtended(Tree sig, FIRIndex const & index)
{
    xtended* p = (xtended*)getUserData(sig);
    vector<Tree> arguments;
    vector<AudioType*> argTypes;
    AudioType* resultType = getSigType(sig);

    for (int i = 0; i < sig->arity(); i++) {
        arguments.push_back(sig->branch(i));
        argTypes.push_back(getSigType(sig->branch(i)));
    }

    // FIXME: for now we ignore the p->needCache flag
    return dispatchPolymorphicFunctor(sig, arguments, index, ScalarXtendedFunctor(p, fContainer, resultType, argTypes), false);
}


struct ScalarFfunFunctor
{
    string fFunctionName;

    ScalarFfunFunctor(string const & functionName):
        fFunctionName(functionName)
    {}

    template <typename ArgIterator>
    ValueInst * operator()(ArgIterator argsBegin, ArgIterator argsEnd) const
    {
        list<ValueInst*> args(argsBegin, argsEnd);

        ValueInst * result = InstBuilder::genFunCallInst(fFunctionName, args);
        return result;
    }
};

ValueInst * MultirateInstructionsCompiler::compileFFun(Tree sig, Tree ff, Tree args, FIRIndex const & index)
{
    fContainer->addIncludeFile(ffincfile(ff));
    fContainer->addLibrary(fflibfile(ff));

    string funname = ffname(ff);
    vector<Tree> arguments;
    list<NamedTyped*> args_types;
    FunTyped* fun_type;

    for (int i = 0; i< ffarity(ff); i++) {
        stringstream num; num << i;
        Tree parameter = nth(args, i);
        ::Type t1 = getSigType(parameter)->getScalarBaseType();
        args_types.push_back(InstBuilder::genNamedTyped("dummy" + num.str(), InstBuilder::genBasicTyped((t1->nature() == kInt) ? Typed::kInt : itfloat())));
        arguments.push_back(parameter);
    }

    // Add function declaration
    fun_type = InstBuilder::genFunTyped(args_types, InstBuilder::genBasicTyped((ffrestype(ff) == kInt) ? Typed::kInt : itfloat()));
    pushExtGlobalDeclare(InstBuilder::genDeclareFunInst(funname, fun_type));

    return dispatchPolymorphicFunctor(sig, arguments, index, ScalarFfunFunctor(funname), false);
}

ValueInst * MultirateInstructionsCompiler::compilePrimitive(Tree sig, FIRIndex const & index)
{
    int     i;
    double  r;
    Tree    c, sel, x, y, z, label, id, ff, largs, type, name, file;

    if (isSigBinOp(sig, &i, x, y))
        return compileBinop(sig, i, x, y, index);

    if (getUserData(sig))
        return compileXtended(sig, index);

    if (isSigFFun(sig, ff, largs))
        return compileFFun(sig, ff, largs, index);


    throw std::runtime_error("not implemented");
}


StatementInst * MultirateInstructionsCompiler::compileAssignmentVectorize(Address * vec, Tree sig,
                                                                          FIRIndex const & index, Tree arg1, Tree arg2)
{
    if (!isShared(sig)) {
        int n = tree2int(arg2);

        ForLoopInst * subloop = genSubloop("k", 0, n);

        IndexedAddress * destination = InstBuilder::genIndexedAddress(vec, subloop->loadDeclaration());
        FIRIndex computeIndex = index * n + subloop->loadDeclaration();

        StatementInst * blockInst = compileAssignment(destination, arg1, computeIndex);
        subloop->pushBackInst(blockInst);
        return subloop;
    } else {
        return store(vec, compileSample(sig, index));
    }
}

ValueInst * MultirateInstructionsCompiler::compileSampleVectorize(Tree sig, FIRIndex const & index, Tree arg1, Tree arg2)
{
    int n = tree2int(arg2);

    int sigRate = getSigRate(sig);
    Typed * sigTyped = declareSignalType(sig);

    ArrayTyped * resultBufferType = declareArrayTyped(sigTyped, sigRate * gVecSize);

    DeclareVarInst * declareResultBuffer = InstBuilder::genDecStackVar(getFreshID("W"), resultBufferType);

    pushDeclare(declareResultBuffer);

    fContainer->openLoop(getFreshID("j_"), sigRate);

    ForLoopInst * subloop = genSubloop("k", 0, n);

    NamedAddress * resultBufferAddress = InstBuilder::genVectorAddress(declareResultBuffer->getName(),
                                                                        resultBufferType->fType,
                                                                        resultBufferType->fSize,
                                                                        Address::kStack);

    IndexedAddress * compileAddress = InstBuilder::genIndexedAddress(InstBuilder::genIndexedAddress(resultBufferAddress,
                                                                                                    getCurrentLoopIndex()),
                                                                     subloop->loadDeclaration());

    FIRIndex compileIndex = getCurrentLoopIndex() * n + subloop->loadDeclaration();

    subloop->pushBackInst(compileAssignment(compileAddress, arg1, compileIndex));

    pushComputeDSPMethod(subloop);
    fContainer->closeLoop();

    IndexedAddress * addressToReturn = InstBuilder::genIndexedAddress(declareResultBuffer->getAddress(), index);
    return InstBuilder::genLoadVarInst(addressToReturn);
}

StatementInst * MultirateInstructionsCompiler::compileAssignmentSerialize(Address * vec, Tree sig,
                                                                          FIRIndex const & index, Tree arg1)
{
    if (!isShared(sig)) {
        int n = getSigRate(sig) / getSigRate(arg1);
        Typed * argType = declareSignalType(arg1);

        CastAddress * castedResultAddress = InstBuilder::genCastAddress(vec, argType);

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
    Typed * sigType = declareSignalType(sig);
    Typed * argType = declareSignalType(arg1);

    int m = getSigRate(arg1);
    int n = getSigRate(sig) / getSigRate(arg1);

    int loopSize = n * m;

    ArrayTyped* resultBufferType = declareArrayTyped(sigType, loopSize);

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
    CastAddress * castedDestiationAddress = InstBuilder::genCastAddress(destinationAddress, argType);

    FIRIndex compileIndex = getCurrentLoopIndex() / n;
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

    Typed* sigType = declareSignalType(sig);
    Typed* argType1 = declareSignalType(arg1);
    Typed* argType2 = declareSignalType(arg2);

    assert(isArrayTyped(sigType) && isArrayTyped(argType1) && isArrayTyped(argType2));
    ArrayTyped * vArgType1 = isArrayTyped(argType1);

    ArrayTyped* resultBufferType = declareArrayTyped(sigType, rate * gVecSize);

    DeclareVarInst * declareResultBuffer = InstBuilder::genDecStackVar(getFreshID("W"), resultBufferType);

    fContainer->openLoop(getFreshID("j_"), rate);

    IndexedAddress * resultAddress1 = InstBuilder::genIndexedAddress(InstBuilder::genIndexedAddress(declareResultBuffer->getAddress(),
                                                                                                    getCurrentLoopIndex()),
                                                                     InstBuilder::genIntNumInst(0));
    IndexedAddress * resultAddress2 = InstBuilder::genIndexedAddress(InstBuilder::genIndexedAddress(declareResultBuffer->getAddress(),
                                                                                                    getCurrentLoopIndex()),
                                                                     InstBuilder::genIntNumInst(vArgType1->fSize));

    CastAddress * castedResultAddress1 = InstBuilder::genCastAddress(resultAddress1, argType1);
    CastAddress * castedResultAddress2 = InstBuilder::genCastAddress(resultAddress2, argType2);

    compileAssignment(castedResultAddress1, arg1, getCurrentLoopIndex());
    compileAssignment(castedResultAddress2, arg2, getCurrentLoopIndex());

    fContainer->closeLoop();

    IndexedAddress * addressToReturn = InstBuilder::genIndexedAddress(declareResultBuffer->getAddress(), index);
    return InstBuilder::genLoadVarInst(addressToReturn);
}

StatementInst * MultirateInstructionsCompiler::compileAssignmentConcat(Address * vec, Tree sig, FIRIndex const & index, Tree arg1, Tree arg2)
{
    if (!isShared(sig)) {
        assert(getSigRate(sig) == getSigRate(arg1) && getSigRate(sig) == getSigRate(arg2));

        Typed* sigType = declareSignalType(sig);
        Typed* argType1 = declareSignalType(arg1);
        Typed* argType2 = declareSignalType(arg2);

        assert(isArrayTyped(sigType) && isArrayTyped(argType1) && isArrayTyped(argType2));
        ArrayTyped * vArgType1 = isArrayTyped(argType1);

        IndexedAddress * resultAddress1 = InstBuilder::genIndexedAddress(vec, InstBuilder::genIntNumInst(0));
        IndexedAddress * resultAddress2 = InstBuilder::genIndexedAddress(vec, InstBuilder::genIntNumInst(vArgType1->fSize));

        CastAddress * castedResultAddress1 = InstBuilder::genCastAddress(resultAddress1, argType1);
        CastAddress * castedResultAddress2 = InstBuilder::genCastAddress(resultAddress2, argType2);

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

static Tree declaredDelayLineProperty = tree(Node("declaredDelayLineProperty"));

static IndexedAddress * getDelayLineDeclaration(Tree delayline)
{
    Tree declaredDelayLine = delayline->getProperty(declaredDelayLineProperty);

    if (declaredDelayLine)
        return static_cast<IndexedAddress*>(tree2ptr(declaredDelayLine));
    else
        return NULL;
}

static int getDelaylineRate(Tree delayedSignal)
{
    //FIXME: maybe we can simply annotate the delayline with a rate?
    int sigRate = getSigRate(delayedSignal);
    if (sigRate)
        return sigRate;

    Tree recursiveGroup;
    int i;
    if (isProj(delayedSignal, &i, recursiveGroup)) {
        Tree id, body;
        ensure(isRec(recursiveGroup, id, body));
        return getDelaylineRate(nth(body, i));
    }
    assert(false);

}

Address * MultirateInstructionsCompiler::declareDelayLine(Tree delayline)
{
    Tree arg;
    ensure (isSigDelayLine(delayline, arg));

    Tree declaredDelayLine = delayline->getProperty(declaredDelayLineProperty);
    if (declaredDelayLine)
        return (Address*)tree2ptr(declaredDelayLine);

    // if small
    int sigRate = getDelaylineRate(arg);
    assert(sigRate);
    Typed * sigType = declareSignalType(arg);
    int maxDelay = getMaxDelay(delayline);
    maxDelay = max(1, maxDelay); // FIXME: we ensure a delay of one sample, later we need to distinguish between delays of
                                 // projections and `normal' delays

    ArrayTyped * mType = declareArrayTyped(sigType, maxDelay);
    ArrayTyped * rmType = declareArrayTyped(sigType, sigRate * gVecSize + maxDelay);

    DeclareVarInst * M = InstBuilder::genDecStructVar(getFreshID("M"), mType);
    DeclareVarInst * RM = InstBuilder::genDecStructVar(getFreshID("RM"), rmType);
    pushDeclare(M);
    pushDeclare(RM);

    static Tree declareM = tree(Node("declareM"));
    static Tree declareRM = tree(Node("declareRM"));

    delayline->setProperty(declareM, tree(Node((void*)M)));
    delayline->setProperty(declareRM, tree(Node((void*)RM)));
    Address * returnAddress = InstBuilder::genIndexedAddress(RM->getAddress(), InstBuilder::genIntNumInst(maxDelay));
    delayline->setProperty(declaredDelayLineProperty, tree(Node((void*)returnAddress)));

    ForLoopInst * clearMLoop = genSubloop("x", 0, maxDelay);
    clearMLoop->pushBackInst(InstBuilder::genStoreVarInst(InstBuilder::genIndexedAddress(M->getAddress(), clearMLoop->loadDeclaration()),
                                                          InstBuilder::genIntNumInst(0)));

    pushInitMethod(clearMLoop);

    return returnAddress;
}

static Tree delayLineLoadLoopProperty = tree(Node("delayLineLoadLoopProperty"));

Address * MultirateInstructionsCompiler::compileDelayline(Tree delayline)
{
    Tree arg;
    ensure (isSigDelayLine(delayline, arg));

    static Tree compiledDelayLineProperty = tree(Node("compiledDelayLineProperty"));

    Tree compiledDelayLine = delayline->getProperty(compiledDelayLineProperty);
    if (compiledDelayLine)
        return (Address*)tree2ptr(compiledDelayLine);

    Address * delayLineAddress = declareDelayLine(delayline);
    static Tree declareM = tree(Node("declareM"));
    static Tree declareRM = tree(Node("declareRM"));

    int sigRate = getDelaylineRate(arg);
    int maxDelay = getMaxDelay(delayline);
    maxDelay = max(1, maxDelay); // FIXME: we ensure a delay of one sample, later we need to distinguish between delays of
                                 // projections and `normal' delays

    DeclareVarInst * M  = (DeclareVarInst *)tree2ptr(delayline->getProperty(declareM));
    DeclareVarInst * RM = (DeclareVarInst *)tree2ptr(delayline->getProperty(declareRM));

    fContainer->openLoop("delStoreIndex", 1);
    fContainer->openLoop("j", sigRate);
    fContainer->openLoop("delLoadIndex", 1);

    // FIXME: we pack the delayload loop into a subloop in order to avoid the rate to be taken into account
    ForLoopInst * delayloadLoop = genSubloop("j", 0, maxDelay);
    LoadVarInst * loadM = InstBuilder::genLoadVarInst(InstBuilder::genIndexedAddress(M->getAddress(),
                                                                                     delayloadLoop->loadDeclaration()));
    StoreVarInst * storeRM = InstBuilder::genStoreVarInst(InstBuilder::genIndexedAddress(RM->getAddress(),
                                                                                         delayloadLoop->loadDeclaration()),
                                                          loadM);
    delayloadLoop->pushBackInst(storeRM);
    pushComputePreDSPMethod(delayloadLoop);

    delayline->setProperty(delayLineLoadLoopProperty, tree(Node((void*)fContainer->getCurLoop())));

    fContainer->closeLoop();

    CodeLoop * delayWriteLoop = fContainer->getCurLoop();


    Address * address = InstBuilder::genIndexedAddress(RM->fAddress, FIRIndex(getCurrentLoopIndex()) + maxDelay);
    pushComputeDSPMethod(compileAssignment(address, arg, getCurrentLoopIndex()));

    fContainer->closeLoop();

    ForLoopInst * writeBackLoop = genSubloop("j", 0, maxDelay);
    FIRIndex loadRMIndex = FIRIndex(writeBackLoop->loadDeclaration()) + (sigRate * gVecSize);
    LoadVarInst * loadRM = InstBuilder::genLoadVarInst(InstBuilder::genIndexedAddress(RM->getAddress(),
                                                                                      loadRMIndex));
    StoreVarInst * storeM = InstBuilder::genStoreVarInst(InstBuilder::genIndexedAddress(M->getAddress(),
                                                                                        writeBackLoop->loadDeclaration()),
                                                         loadRM);
    writeBackLoop->pushBackInst(storeM);

    // FIXME: we pack the writeback loop into a subloop in order to avoid the rate to be taken into account
    pushComputePostDSPMethod(writeBackLoop);

    fContainer->closeLoop();

    setLoopProperty(delayline, delayWriteLoop);
    delayline->setProperty(compiledDelayLineProperty, tree(Node((void*)delayLineAddress)));

    return delayLineAddress;
}

ValueInst * MultirateInstructionsCompiler::compileSampleDelay(Tree sig, FIRIndex const & index, Tree delayline, Tree delay)
{
    IndexedAddress * delayAddress = getDelayLineDeclaration(delayline);

    if (delayAddress == NULL)
        // we need to compile the delayline
        delayAddress = dynamic_cast<IndexedAddress*>(compileDelayline(delayline));

    ValueInst * compiledDelayLength = compileSample(delay, index);

    FIRIndex indexInDelayline = index - compiledDelayLength;
    LoadVarInst * loadDelay = InstBuilder::genLoadVarInst(InstBuilder::genIndexedAddress(delayAddress->fAddress,
                                                                                         indexInDelayline + delayAddress->fIndex));

    // FIXME: maybe we can annotate the delay, if it is inside a recursion or outside?
    // set loop dependency explicitly
    CodeLoop * delaylineLoop;
    if (getLoopProperty(delayline, delaylineLoop))
        // the delayline has a loop property, so we are outside of a recursion and can add a dependency
        fContainer->getCurLoop()->addBackwardDependency(delaylineLoop);
    else {
        // we need to ensure that the delay line has been loaded
        CodeLoop * loadLoop = (CodeLoop *)tree2ptr(delayline->getProperty(delayLineLoadLoopProperty));
        assert(loadLoop);
        fContainer->getCurLoop()->addBackwardDependency(loadLoop);
    }
    return loadDelay;
}

StatementInst * MultirateInstructionsCompiler::compileAssignmentProjection(Address * vec, Tree sig, FIRIndex const & index,
                                                                           int projectionIndex, Tree recursiveGroup)
{
    Tree var, listOfExpressions;

    ensure(isRec(recursiveGroup, var, listOfExpressions));
    int numberOfExpressions = len(listOfExpressions);

    assert(projectionIndex < numberOfExpressions);

    Tree expressionToCompute = nth(listOfExpressions, projectionIndex);
    ValueInst * compiledExpression = compileSample(expressionToCompute, index);
    StatementInst * storeInst = store(vec, compiledExpression);
    return storeInst;
}


StatementInst * MultirateInstructionsCompiler::store (Address * address, ValueInst * value)
{
    return InstBuilder::genStoreVarInst(address, value);
}

Typed * MultirateInstructionsCompiler::declareSignalType(Tree sig)
{
    AudioType * type = getSigType(sig);
    return declareSignalType(type);
}

Typed * MultirateInstructionsCompiler::declareSignalType(AudioType * type)
{
    DeclareTypeInst* declareType = InstBuilder::genType(type);
    pushGlobalDeclare(declareType);
    return declareType->fType;
}

Typed * MultirateInstructionsCompiler::declareSignalType(Typed * type)
{
    pushGlobalDeclare(InstBuilder::genDeclareTypeInst(type));
    return type;
}


ArrayTyped * MultirateInstructionsCompiler::declareArrayTyped(Typed * typed, int size)
{
    ArrayTyped* resultBufferType = InstBuilder::genArrayTyped(typed, size);
    DeclareTypeInst * ret = InstBuilder::genDeclareTypeInst(resultBufferType);
    pushGlobalDeclare(ret);
    return resultBufferType;
}



ForLoopInst* MultirateInstructionsCompiler::genSubloop(string const & loopSymbol, int lowBound, int highBound)
{
    DeclareVarInst* loop_decl = InstBuilder::genDecLoopVar(getFreshID(loopSymbol), InstBuilder::genBasicTyped(Typed::kInt),
                                                        InstBuilder::genIntNumInst(lowBound));
    ValueInst* loop_end = InstBuilder::genLessThan(loop_decl->load(), InstBuilder::genIntNumInst(highBound));
    StoreVarInst* loop_increment = loop_decl->store(InstBuilder::genAdd(loop_decl->load(), 1));
    ForLoopInst* loop = InstBuilder::genForLoopInst(loop_decl, loop_end, loop_increment);
    return loop;
}
