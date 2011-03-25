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

void MultirateInstructionsCompiler::compileSingleSignal(Tree L)
{
    Tree sig = prepare(L);     // Optimize, share and annotate expression

    Typed::VarType containerVarType = (fContainer->getSubContainerType() == kInt) ? Typed::kInt
                                                                                  : itfloat();

    Typed* type = declareArrayTyped(InstBuilder::genBasicTyped(containerVarType), 0);

    string name1 = subst("fOutput$0_ptr", T(0));
    string name2 = subst("fOutput$0", T(0));
    pushDeclare(InstBuilder::genDecStructVar(name1, type));
    pushComputeBlockMethod(InstBuilder::genStoreStructVar(name1,
        InstBuilder::genLoadFunArgsVar("output")));
    pushDeclare(InstBuilder::genDecStructVar(name2, type));


    fVectorSize = InstBuilder::genIntNumInst(gVecSize);

    int rate = getSigRate(sig);
    fContainer->setOutputRate(0, rate);

    string outputName(name2);
    NamedAddress * outI = InstBuilder::genNamedAddress(outputName, Address::kStruct, InstBuilder::genArrayTyped(InstBuilder::genBasicTyped(Typed::kFloat), 0));

    compileVector(outI, sig);

    generateUserInterfaceTree(prepareUserInterfaceTree(fUIRoot));
    generateMacroInterfaceTree("", prepareUserInterfaceTree(fUIRoot));
    if (fDescription)
        fDescription->ui(prepareUserInterfaceTree(fUIRoot));

    fContainer->processFIR();
}

CodeContainer* MultirateInstructionsCompiler::signal2Container(const string& name, Tree sig)
{
    Type t = getSigType(sig);

    CodeContainer* container = fContainer->createInternalContainer(name, t->nature());
    MultirateInstructionsCompiler C(container);
    C.compileSingleSignal(sig);
    return container;
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

        NamedAddress * outI = InstBuilder::genNamedAddress(outputName, Address::kStruct, InstBuilder::genArrayTyped(InstBuilder::genBasicTyped(Typed::kFloat), 0));

        compileVector(outI, sig);
    }
}

void MultirateInstructionsCompiler::compileVector(NamedAddress * vec, Tree sig)
{
    int sigRate = getSigRate(sig);

    fContainer->openLoop("j", sigRate);
    FIRIndex index(getCurrentLoopIndex());
    IndexedAddress * storeAddress = InstBuilder::genIndexedAddress(vec, index);
    pushComputeDSPMethod(compileAssignment(storeAddress, sig, index));
    fContainer->closeLoop();
}

static bool isPrimitive(Tree sig)
{
    int     i;
    Tree    c, sel, x, y, z, label, ff, largs, type, name, file;

    if (   getUserData(sig)
        || isSigBinOp(sig, &i, x, y)
        || isSigFFun(sig, ff, largs)
        || isSigFConst(sig, type, name, file)
        || isSigFVar(sig, type, name, file)

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
        || isSigHBargraph(sig, label,x,y,z))
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
        return store(dest, compileSamplePrimitive(sig, index));

    if (isSigFixDelay(sig, arg1, arg2))
        return store(dest, compileSampleDelay(sig, index, arg1, arg2));

    if (isProj(sig, &i, arg2))
        return compileAssignmentProjection(dest, sig, index, i, arg2);

    Tree table, tableIndex;
    if (isSigRDTbl(sig, table, tableIndex))
        return store(dest, compileSampleRDTable(sig, index, table, tableIndex));

    Tree tableID, tableWriteSignal;
    if (isSigWRTbl(sig, tableID, table, tableIndex, tableWriteSignal))
        return store(dest, compileSampleWRTable(sig, index, table, tableID, tableWriteSignal));

    if (isSigAttach(sig, arg1, arg2)) {
        compileSample(arg2, index);
        return compileAssignment(dest, arg1, index);
    }

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

    Tree table, tableIndex;
    if (isSigRDTbl(sig, table, tableIndex))
        return compileSampleRDTable(sig, index, table, tableIndex);

    Tree tableID, tableWriteSignal;
    if (isSigWRTbl(sig, tableID, table, tableIndex, tableWriteSignal))
        return compileSampleWRTable(sig, index, table, tableID, tableWriteSignal);

    if (isSigAttach(sig, arg1, arg2)) {
        compileSample(arg2, index);
        return compileSample(arg1, index);
    }

    throw std::runtime_error("not implemented");
    return NULL;
}

ValueInst * MultirateInstructionsCompiler::compileSampleInput(Tree sig, int i, FIRIndex const & index)
{
    int rate = getSigRate(sig);
    fContainer->setInputRate(i, rate);

    string name = subst("fInput$0", T(i));
    LoadVarInst * res = InstBuilder::genLoadArrayStructVar(name, index);

    ValueInst * castedToFloat = InstBuilder::genCastNumInst(res, InstBuilder::genBasicTyped(itfloat()));
    return castedToFloat;
}


ValueInst * MultirateInstructionsCompiler::compileSamplePrimitive(Tree sig, FIRIndex const & index)
{
    if (isShared(sig)) {
        const int rate = getSigRate(sig);
        bool isBlockRate = (rate == 0);

        FIRIndex returnIndex = isBlockRate ? FIRIndex(InstBuilder::genIntNumInst(0))
                                           : index;

        ValueInst * alreadyCompiledExpression;
        if (getCompiledExpression(sig, alreadyCompiledExpression)) {
            LoadVarInst * bufferHandle = dynamic_cast<LoadVarInst*>(alreadyCompiledExpression);
            IndexedAddress * addressToReturn = InstBuilder::genIndexedAddress(bufferHandle->fAddress, returnIndex);
            return InstBuilder::genLoadVarInst(addressToReturn);
        }

        const int subloobSize = isBlockRate ? 1 : rate;
        const int cacheBufferSize = isBlockRate ? 1 : rate * gVecSize;

        Typed * sampleTyped = declareSignalType(sig);
        ArrayTyped* sampleArrayType = declareArrayTyped(sampleTyped, cacheBufferSize);

        DeclareVarInst * declareCacheBuffer = InstBuilder::genDecStackVar(getFreshID("cacheVector"), sampleArrayType);
        pushDeclare(declareCacheBuffer);
        setCompiledExpression(sig, declareCacheBuffer->load());

        fContainer->openLoop("j", subloobSize);
        if (isBlockRate) {
            IndexedAddress * storeAddress = InstBuilder::genIndexedAddress(declareCacheBuffer->getAddress(), FIRIndex(0));
            pushComputePreDSPMethod(store(storeAddress, compilePrimitive(sig, FIRIndex(0))));
        } else {
            IndexedAddress * storeAddress = InstBuilder::genIndexedAddress(declareCacheBuffer->getAddress(), getCurrentLoopIndex());
            pushComputeDSPMethod(store(storeAddress, compilePrimitive(sig, getCurrentLoopIndex())));
        }
        fContainer->closeLoop();

        IndexedAddress * addressToReturn = InstBuilder::genIndexedAddress(declareCacheBuffer->getAddress(), returnIndex);
        return InstBuilder::genLoadVarInst(addressToReturn);

    } else {
        return compilePrimitive(sig, index);
    }
}

void MultirateInstructionsCompiler::setCompiledCache(Tree sig, LoadVarInst * loadCacheInst)
{
    setCompiledExpression(sig, loadCacheInst);
    setLoopProperty(sig, fContainer->getCurLoop());
}

ValueInst * MultirateInstructionsCompiler::getCompiledCache(Tree sig, FIRIndex const & index)
{
    ValueInst * compiledExpression;
    if (getCompiledExpression(sig, compiledExpression)) {
        LoadVarInst * bufferHandle = dynamic_cast<LoadVarInst*>(compiledExpression);
        Address * cacheAddress = bufferHandle->fAddress;

        CodeLoop * loop;
        ensure(getLoopProperty(sig, loop));
        fContainer->getCurLoop()->addBackwardDependency(loop);

        const int rate = getSigRate(sig);
        bool isBlockRate = (rate == 0);

        FIRIndex returnIndex = isBlockRate ? FIRIndex(InstBuilder::genIntNumInst(0))
                                           : index;

        return InstBuilder::genLoadVarInst(InstBuilder::genIndexedAddress(cacheAddress, returnIndex));
    } else
        return NULL;
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

ValueInst * MultirateInstructionsCompiler::loadForeignVar(Tree sig, Tree type, Tree name, Tree file, FIRIndex const & index)
{
    string nameString = tree2str(name);

    fContainer->addIncludeFile(tree2str(file));

    int sig_type = getSigType(sig)->nature();
    pushExtGlobalDeclare(InstBuilder::genDecGlobalVar(nameString,
        InstBuilder::genBasicTyped((sig_type == kInt) ? Typed::kInt : itfloat())));
    return InstBuilder::genLoadGlobalVar(nameString);
}


struct CastFunctor
{
    const int fNature;

    CastFunctor(int nature):
        fNature(nature)
    {}

    template <typename ArgIterator>
    ValueInst * operator()(ArgIterator argsBegin, ArgIterator argsEnd) const
    {
        const size_t argumentCount = argsEnd - argsBegin;
        assert(argumentCount == 1);
        ValueInst * arg = *argsBegin;

        if (fNature == Typed::kInt)
            return InstBuilder::genCastNumInst(arg, InstBuilder::genBasicTyped(Typed::kInt));
        else
            return InstBuilder::genCastNumInst(arg, InstBuilder::genBasicTyped(itfloat()));
    }
};

ValueInst * MultirateInstructionsCompiler::compileCast(Tree sig, Tree arg, Typed::VarType type, FIRIndex const & index)
{
    vector<Tree> arguments;
    arguments.push_back(arg);

    return dispatchPolymorphicFunctor(sig, arguments, index, CastFunctor(type), false);
}

ValueInst * MultirateInstructionsCompiler::compileSelect2(Tree sig, Tree selector, Tree x, Tree y, FIRIndex const & index)
{
    int t1 = getSigType(x)->nature();
    int t2 = getSigType(y)->nature();

    ValueInst* compiledSelector = compileSample(selector, index);
    ValueInst* compiledX = compileSample(x, index);
    ValueInst* compiledY = compileSample(y, index);

    if (t1 == kReal &&
        t2 == kInt)
        compiledY = InstBuilder::genCastNumInst(compiledY, InstBuilder::genBasicTyped(itfloat()));

    if (t1 == kInt &&
        t2 == kReal)
        compiledX = InstBuilder::genCastNumInst(compiledX, InstBuilder::genBasicTyped(itfloat()));

    return InstBuilder::genSelect2Inst(compiledSelector, compiledX, compiledY);
}

ValueInst * MultirateInstructionsCompiler::compileSelect3(Tree sig, Tree selector, Tree x, Tree y, Tree z,
                                                          FIRIndex const & index)
{
    throw std::runtime_error("internal error: Select3 instruction does not exist, yet");
}

ValueInst * MultirateInstructionsCompiler::compileButton(Tree sig, Tree path, const string & name, FIRIndex const & index)
{
    string varname = getFreshID(name);
    Typed* type = InstBuilder::genBasicTyped(Typed::kFloatMacro);

    pushDeclare(InstBuilder::genDecStructVar(varname, type));
    pushInitMethod(InstBuilder::genStoreStructVar(varname, InstBuilder::genRealNumInst(Typed::kFloatMacro, 0)));
    addUIWidget(reverse(tl(path)), uiWidget(hd(path), tree(varname), sig));

    return InstBuilder::genCastNumInst(InstBuilder::genLoadStructVar(varname), InstBuilder::genBasicTyped(itfloat()));
}

ValueInst* MultirateInstructionsCompiler::compileSlider(Tree sig, Tree path, Tree cur, Tree min, Tree max, Tree step,
                                                        const string& name, FIRIndex const & index)
{
    string varname = getFreshID(name);
    Typed* type = InstBuilder::genBasicTyped(Typed::kFloatMacro);

    pushDeclare(InstBuilder::genDecStructVar(varname, type));
    pushInitMethod(InstBuilder::genStoreStructVar(varname, InstBuilder::genRealNumInst(Typed::kFloatMacro, tree2float(cur))));
    addUIWidget(reverse(tl(path)), uiWidget(hd(path), tree(varname), sig));

    return InstBuilder::genCastNumInst(InstBuilder::genLoadStructVar(varname), InstBuilder::genBasicTyped(itfloat()));
}

ValueInst* MultirateInstructionsCompiler::compileBargraph(Tree sig, Tree path, Tree min, Tree max, Tree value,
                                                          const string& name, FIRIndex const & index)
{
    string varname = getFreshID(name);
    pushDeclare(InstBuilder::genDecStructVar(varname, InstBuilder::genBasicTyped(Typed::kFloatMacro)));
    addUIWidget(reverse(tl(path)), uiWidget(hd(path), tree(varname), sig));

    ValueInst * exp = compileSample(value, index);

    ::Type t = getSigType(sig);
    switch (t->variability()) {
        case kKonst :
            pushInitMethod(InstBuilder::genStoreStructVar(varname, exp));
            break;
        case kBlock :
            pushComputeBlockMethod(InstBuilder::genStoreStructVar(varname, exp));
            break;

        case kSamp :
            pushComputeDSPMethod(InstBuilder::genStoreStructVar(varname, exp));
            break;
    }

    return InstBuilder::genCastNumInst(InstBuilder::genLoadStructVar(varname), InstBuilder::genBasicTyped(itfloat()));
}


ValueInst * MultirateInstructionsCompiler::compilePrimitive(Tree sig, FIRIndex const & index)
{
    int     i;
    Tree    c, sel, x, y, z, label, ff, largs, type, name, file;

    if (isSigBinOp(sig, &i, x, y))
        return compileBinop(sig, i, x, y, index);

    if (getUserData(sig))
        return compileXtended(sig, index);

    if (isSigFFun(sig, ff, largs))
        return compileFFun(sig, ff, largs, index);

    if (isSigFConst(sig, type, name, file) ||
        isSigFVar(sig, type, name, file) )
        return loadForeignVar(sig, type, name, file, index);

    if (isSigIntCast(sig, x))
        return compileCast(sig, x, Typed::kInt, index);
    if (isSigFloatCast(sig, x))
        return compileCast(sig, x, itfloat(), index);

    if (isSigSelect2(sig, sel, x, y))
        return compileSelect2(sig, sel, x, y, index);

    if (isSigSelect3(sig, sel, x, y, z))
        return compileSelect3(sig, sel, x, y, z, index);

    if (isSigPrefix(sig, x, y))
        throw std::runtime_error("sigPrefix is not implemented, yet");

    if (isSigButton(sig, label))
        return compileButton(sig, label, "fbutton", index);
    if (isSigCheckbox(sig, label))
        return compileButton(sig, label, "fcheckbox", index);

    if (isSigVSlider(sig, label, c, x, y, z))
        return compileSlider(sig, label, c, x, y, z, "fvslider", index);
    if (isSigHSlider(sig, label, c, x, y, z))
        return compileSlider(sig, label, c, x, y, z, "fhslider", index);
    if (isSigNumEntry(sig, label, c, x, y, z))
        return compileSlider(sig, label, c, x, y, z, "fentry", index);

    if (isSigVBargraph(sig, label, x, y, z))
        return compileBargraph(sig, label, x, y, z, "fvbargraph", index);
    if (isSigHBargraph(sig, label, x, y, z))
        return compileBargraph(sig, label, x, y, z, "fhbargraph", index);

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
    ValueInst * alreadyCompiledExpression;
    if (getCompiledExpression(sig, alreadyCompiledExpression)) {
        LoadVarInst * bufferHandle = dynamic_cast<LoadVarInst*>(alreadyCompiledExpression);
        IndexedAddress * addressToReturn = InstBuilder::genIndexedAddress(bufferHandle->fAddress, index);
        return InstBuilder::genLoadVarInst(addressToReturn);
    }


    int n = tree2int(arg2);

    int sigRate = getSigRate(sig);
    Typed * sigTyped = declareSignalType(sig);

    ArrayTyped * resultBufferType = declareArrayTyped(sigTyped, sigRate * gVecSize);

    DeclareVarInst * declareResultBuffer = InstBuilder::genDecStackVar(getFreshID("W"), resultBufferType);
    pushDeclare(declareResultBuffer);
    setCompiledExpression(sig, declareResultBuffer->load()); // cache the load handle to the result buffer

    fContainer->openLoop("j", sigRate);

    ForLoopInst * subloop = genSubloop("k", 0, n);

    NamedAddress * resultBufferAddress = InstBuilder::genNamedAddress(declareResultBuffer->getName(),
                                                                      Address::kStack,
                                                                      resultBufferType);

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
    if (true || !isShared(sig)) {
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
    ValueInst * alreadyCompiledExpression;
    if (getCompiledExpression(sig, alreadyCompiledExpression)) {
        LoadVarInst * bufferHandle = dynamic_cast<LoadVarInst*>(alreadyCompiledExpression);
        IndexedAddress * addressToReturn = InstBuilder::genIndexedAddress(bufferHandle->fAddress, index);
        return InstBuilder::genLoadVarInst(addressToReturn);
    }

    Typed * sigType = declareSignalType(sig);
    Typed * argType = declareSignalType(arg1);

    int argumentRate = getSigRate(arg1);    // m
    int sigRate = getSigRate(sig);          // n*m
    int rateFactor = sigRate / argumentRate;

    ArrayTyped* resultBufferType = declareArrayTyped(sigType, sigRate * gVecSize);

    DeclareVarInst * declareResultBuffer = InstBuilder::genDecStackVar(getFreshID("W"), resultBufferType);
    pushDeclare(declareResultBuffer);
    setCompiledExpression(sig, declareResultBuffer->load()); // cache the load handle to the result buffer

    fContainer->openLoop("j", sigRate);

    ValueInst * condition = InstBuilder::genBinopInst(kEQ,
                                                      InstBuilder::genBinopInst(kRem, getCurrentLoopIndex(), InstBuilder::genIntNumInst(rateFactor)),
                                                      InstBuilder::genIntNumInst(0));


    NamedAddress * resultBufferAddress = InstBuilder::genNamedAddress(declareResultBuffer->getName(),
                                                                         Address::kStack,
                                                                        resultBufferType);

    IndexedAddress * destinationAddress = InstBuilder::genIndexedAddress(resultBufferAddress,
                                                                         getCurrentLoopIndex());
    CastAddress * castedDestiationAddress = InstBuilder::genCastAddress(destinationAddress, argType);

    FIRIndex compileIndex = getCurrentLoopIndex() / sigRate;
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
    ValueInst * alreadyCompiledExpression;
    if (getCompiledExpression(sig, alreadyCompiledExpression)) {
        LoadVarInst * bufferHandle = dynamic_cast<LoadVarInst*>(alreadyCompiledExpression);
        IndexedAddress * addressToReturn = InstBuilder::genIndexedAddress(bufferHandle->fAddress, index);
        return InstBuilder::genLoadVarInst(addressToReturn);
    }

    int rate = getSigRate(sig);
    assert(getSigRate(sig) == getSigRate(arg1) && getSigRate(sig) == getSigRate(arg2));

    Typed* sigType = declareSignalType(sig);
    Typed* argType1 = declareSignalType(arg1);
    Typed* argType2 = declareSignalType(arg2);

    assert(isArrayTyped(sigType) && isArrayTyped(argType1) && isArrayTyped(argType2));
    ArrayTyped * vArgType1 = isArrayTyped(argType1);

    ArrayTyped* resultBufferType = declareArrayTyped(sigType, rate * gVecSize);

    DeclareVarInst * declareResultBuffer = InstBuilder::genDecStackVar(getFreshID("W"), resultBufferType);
    pushDeclare(declareResultBuffer);
    setCompiledExpression(sig, declareResultBuffer->load()); // cache the load handle to the result buffer

    fContainer->openLoop("j", rate);

    IndexedAddress * resultAddress1 = InstBuilder::genIndexedAddress(InstBuilder::genIndexedAddress(declareResultBuffer->getAddress(),
                                                                                                    getCurrentLoopIndex()),
                                                                     InstBuilder::genIntNumInst(0));
    IndexedAddress * resultAddress2 = InstBuilder::genIndexedAddress(InstBuilder::genIndexedAddress(declareResultBuffer->getAddress(),
                                                                                                    getCurrentLoopIndex()),
                                                                     InstBuilder::genIntNumInst(vArgType1->fSize));

    CastAddress * castedResultAddress1 = InstBuilder::genCastAddress(resultAddress1, argType1);
    CastAddress * castedResultAddress2 = InstBuilder::genCastAddress(resultAddress2, argType2);

    pushComputeDSPMethod(compileAssignment(castedResultAddress1, arg1, getCurrentLoopIndex()));
    pushComputeDSPMethod(compileAssignment(castedResultAddress2, arg2, getCurrentLoopIndex()));

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

    // block-rate signals have a rate of one
    AudioType * delayedSignalType = getSigType(delayedSignal);
    assert(delayedSignalType->variability() < kSamp);
    return 1;
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
    IndexedAddress * storeAddress = InstBuilder::genIndexedAddress(M->getAddress(), clearMLoop->loadDeclaration());

    vector<int> dimensions = mType->dimensions();
    dimensions.pop_back();
    vector<ForLoopInst*> forLoopStack;
    forLoopStack.push_back(clearMLoop);

    for (size_t i = 0; i != dimensions.size(); ++i) {
        ForLoopInst * subLoop = genSubloop("k", 0, dimensions[i]);
        forLoopStack.back()->pushBackInst(subLoop);
        forLoopStack.push_back(subLoop);
        storeAddress = InstBuilder::genIndexedAddress(storeAddress, subLoop->loadDeclaration());
    }

    forLoopStack.back()->pushBackInst(InstBuilder::genStoreVarInst(storeAddress,
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


    Address * address = InstBuilder::genIndexedAddress(RM->fAddress, FIRIndex(getCurrentLoopIndex()) + maxDelay);
    pushComputeDSPMethod(compileAssignment(address, arg, getCurrentLoopIndex()));

    fContainer->closeLoop();

    CodeLoop * delayWriteLoop = fContainer->getCurLoop();


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


ValueInst * MultirateInstructionsCompiler::compileSampleRDTable(Tree sig, FIRIndex const & index, Tree table, Tree tableIndex)
{
    NamedAddress * tableAddress;

    Tree tableID, tableSize, tableInitializationSignal, writeTable, writeIndex, writeSignal;
    if (isSigTable(table, tableID, tableSize, tableInitializationSignal))
        tableAddress = generateTable(table, tableID, tableSize, tableInitializationSignal, true);
    else if (isSigWRTbl(table, tableID, writeTable, writeIndex, writeSignal)) {
        ValueInst * tableInst = compileSampleWRTable(table, index, writeTable, writeIndex, writeSignal);
        LoadVarInst* loadTable = dynamic_cast<LoadVarInst*>(tableInst);
        tableAddress = dynamic_cast<NamedAddress*>(loadTable->fAddress);
    } else
        throw std::logic_error("the table signal needs to be a sigTable or a sigWRTbl");

    ValueInst * readIndex = compileSample(tableIndex, index);
    IndexedAddress * readAddress = InstBuilder::genIndexedAddress(tableAddress, readIndex);
    return InstBuilder::genLoadVarInst(readAddress);
}

ValueInst * MultirateInstructionsCompiler::compileSampleWRTable(Tree sig, FIRIndex const & index, Tree table,
                                                                Tree writeIndex, Tree writeStream)
{
    Tree tableID, tableSize, tableInitializationSignal;
    ensure (isSigTable(table, tableID, tableSize, tableInitializationSignal));
    NamedAddress * tableAddress = generateTable(table, tableID, tableSize, tableInitializationSignal, false);

    ValueInst * compiledWriteIndex  = compileSample(writeIndex, index);
    ValueInst * compiledWriteStream = compileSample(writeStream, index);

    IndexedAddress * writeAddress = InstBuilder::genIndexedAddress(tableAddress, compiledWriteIndex);

    pushComputeDSPMethod(InstBuilder::genStoreVarInst(writeAddress, compiledWriteStream));

    return InstBuilder::genLoadVarInst(tableAddress);
}

NamedAddress * MultirateInstructionsCompiler::generateTable(Tree table, Tree tableID, Tree tableSize,
                                                            Tree tableInitializationSignal, bool canBeShared)
{
    // TODO: tables can be shared under two conditions:
    //       - they are never written
    //       - the content does not depend on the sampling rate argument

    ValueInst * alreadyCompiledExpression;
    if (getCompiledExpression(table, alreadyCompiledExpression)) {
        LoadVarInst * bufferHandle = dynamic_cast<LoadVarInst*>(alreadyCompiledExpression);
        NamedAddress * tableAddress = dynamic_cast<NamedAddress*>(bufferHandle->fAddress);
        return tableAddress;
    }

    AudioType * signalType = getSigType(table);
    TableType * tableSignalType = isTableType(signalType);
    AudioType * contentType = tableSignalType->content();
    Typed * signalTyped = declareSignalType(contentType);
    assert(contentType == getSigType(tableInitializationSignal).pointee());

    int iTableSize;
    ensure(isSigInt(tableSize, &iTableSize));

    Tree tableContent;
    ensure(isSigGen(tableInitializationSignal, tableContent));

    Typed * tableTyped = declareArrayTyped(signalTyped, iTableSize);
    string tableName = getFreshID("table_");
    DeclareVarInst * declareTable = InstBuilder::genDecStructVar(tableName, tableTyped);
    pushDeclare(declareTable);
    setCompiledExpression(table, declareTable->load());

    string className = tableName + "Generator";
    CodeContainer * subContainer = signal2Container(className, tableContent);
    fContainer->addSubContainer(subContainer);

    // allocate, use and delete object
    string sigName = tableName + "Sig";

    // allocate
    const list<ValueInst*> allocationArgs;
    DeclareVarInst * declareInitializationObject = InstBuilder::genDecStackVar(sigName,
                                                                               InstBuilder::genNamedTyped(className,
                                                                                                          InstBuilder::genBasicTyped(Typed::kObj_ptr)),
                                                                               InstBuilder::genFunCallInst("new" + className,
                                                                                                           allocationArgs));
    pushInitMethod(declareInitializationObject);

    // initialize
    list<ValueInst*> initializationArgs;
    initializationArgs.push_back(declareInitializationObject->load());
    initializationArgs.push_back(InstBuilder::genLoadFunArgsVar("samplingFreq"));
    pushInitMethod(InstBuilder::genDropInst(InstBuilder::genFunCallInst("instanceInit" + className, initializationArgs)));

    // fill
    list<ValueInst*> fillArgs;
    fillArgs.push_back(declareInitializationObject->load());
    fillArgs.push_back(InstBuilder::genIntNumInst(iTableSize));
    fillArgs.push_back(declareTable->load());
    pushInitMethod(InstBuilder::genDropInst(InstBuilder::genFunCallInst("fill" + className, fillArgs)));

    // destroy
    list<ValueInst*> destructionArgs;
    destructionArgs.push_back(InstBuilder::genLoadStackVar(sigName));
    pushPostInitMethod(InstBuilder::genDropInst(InstBuilder::genFunCallInst("delete" + className, destructionArgs)));

    NamedAddress * returnAddress = dynamic_cast<NamedAddress*>(declareTable->fAddress);
    return returnAddress;
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
