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

#ifndef _INSTRUCTION_COMPILER_BASE_HH
#define _INSTRUCTION_COMPILER_BASE_HH

#include <string>
#include <list>
#include <set>
#include <map>
#include <vector>

#include "instructions.hh"
#include "code_container.hh"
#include "occurences.hh"
#include "property.hh"
#include "Text.hh"

using namespace std;
extern string gMasterName;

/* base class of an instructions compiler:
 * - provides a code container
 * - provides functionality to prepare signals
 * - provides compiler-specific properties
 * - maintains identifies generation
 * - maintains Description
 * - gui
 *
 */
class InstructionsCompilerBase {
    typedef ValueInst* InstType;

protected:
    CodeContainer* fContainer;
    Tree fUIRoot;
    std::set<DeclareTypeInst*> fGlobalTypeDeclarations;

    property<ValueInst*> fCompileProperty;
    property<string> fVectorProperty;
    property<pair<string, string> > fStaticInitProperty;
    property<pair<string,string> > fInstanceInitProperty;
    property<string> fTableProperty;
    static map<string, int> fIDCounters;
    Tree fSharingKey;
    OccMarkup fOccMarkup;

    Description* fDescription;

    void getTypedNames(::Type t, const string& prefix, Typed::VarType& ctype, string& vname);

    bool getCompiledExpression(Tree sig, InstType& cexp);
    InstType setCompiledExpression(Tree sig, const InstType& cexp);

    void setVectorNameProperty(Tree sig, const string& vecname);
    bool getVectorNameProperty(Tree sig, string& vecname);

    void setTableNameProperty(Tree sig, const string& vecname);
    bool getTableNameProperty(Tree sig, string& vecname);

    void setLoopProperty(Tree sig, CodeLoop* l)  { fContainer->setLoopProperty(sig, l); }
    bool getLoopProperty(Tree sig, CodeLoop*& l) { return fContainer->getLoopProperty(sig, l); }


    /* wrapper functions to access code container */
    StatementInst* pushInitMethod(StatementInst* inst)              { return fContainer->pushInitMethod(inst); }
    StatementInst* pushPostInitMethod(StatementInst* inst)          { return fContainer->pushPostInitMethod(inst); }
    StatementInst* pushFrontInitMethod(StatementInst* inst)         { return fContainer->pushFrontInitMethod(inst); }
    StatementInst* pushDestroyMethod(StatementInst* inst)           { return fContainer->pushDestroyMethod(inst); }
    StatementInst* pushStaticInitMethod(StatementInst* inst)        { return fContainer->pushStaticInitMethod(inst); }
    StatementInst* pushPostStaticInitMethod(StatementInst* inst)    { return fContainer->pushPostStaticInitMethod(inst); }
    StatementInst* pushComputeBlockMethod(StatementInst* inst)      { return fContainer->pushComputeBlockMethod(inst); }
    StatementInst* pushUserInterfaceMethod(StatementInst* inst)     { return fContainer->pushUserInterfaceMethod(inst); }

    StatementInst* pushDeclare(StatementInst* inst)                 { return fContainer->pushDeclare(inst); }
    StatementInst* pushGlobalDeclare(StatementInst* inst)
    {
        /* avoid duplicate type declarations */
        DeclareTypeInst * declareType = dynamic_cast<DeclareTypeInst*>(inst);
        if (declareType) {
            if (fGlobalTypeDeclarations.find(declareType) != fGlobalTypeDeclarations.end())
                return inst;
            fGlobalTypeDeclarations.insert(declareType);

            ArrayTyped * declaredArrayType = dynamic_cast<ArrayTyped*>(declareType->fType);
            if (declaredArrayType)
                // ensure that the arrayed type is declared
                pushGlobalDeclare(InstBuilder::genDeclareTypeInst(declaredArrayType->fType));
        }
        return fContainer->pushGlobalDeclare(inst);
    }
    StatementInst* pushExtGlobalDeclare(StatementInst* inst)        { return fContainer->pushExtGlobalDeclare(inst); }

    StatementInst* pushComputePreDSPMethod(StatementInst* inst)     { return fContainer->pushComputePreDSPMethod(inst); }
    StatementInst* pushComputeDSPMethod(StatementInst* inst)        { return fContainer->pushComputeDSPMethod(inst); }
    StatementInst* pushComputePostDSPMethod(StatementInst* inst)    { return fContainer->pushComputePostDSPMethod(inst); }

    int pow2limit(int x)
    {
        int n = 2;
        while (n < x) { n = 2*n; }
        return n;
    }

    string getFreshID(const string& prefix);
    int getSharingCount(Tree sig);
    void setSharingCount(Tree sig, int count);
    void sharingAnalysis(Tree t);
    void sharingAnnotation(int vctxt, Tree sig);
    bool isShared(Tree sig)
    {
        return getSharingCount(sig) > 1;
    }


    Tree prepare(Tree LS);
    Tree prepare2(Tree L0);

    InstructionsCompilerBase(CodeContainer* container)
        :fContainer(container), fUIRoot(uiFolder(cons(tree(0),
            tree(subst("$0", gMasterName))))), fDescription(0)
    {}

    virtual ~InstructionsCompilerBase()
    {}

    // Gestion de la description arborescente de l'IU
    void addUIWidget(Tree path, Tree widget);
    Tree prepareUserInterfaceTree(Tree t);
    void generateUserInterfaceTree(Tree t);
    void generateUserInterfaceElements(Tree elements);
    void generateWidgetCode(Tree fulllabel, Tree varname, Tree sig);

    void generateMacroInterfaceTree(const string& pathname, Tree t);
    void generateMacroInterfaceElements(const string& pathname, Tree elements);
    void generateWidgetMacro(const string& pathname, Tree fulllabel, Tree varname, Tree sig);

    ValueInst * getCurrentLoopIndex(void)
    {
        return fContainer->getCurLoop()->getLoopIndex();
    }

public:
    virtual void compileMultiSignal(Tree rootSignal) = 0;

    void setDescription(Description* descr) { fDescription= descr; }
    Description* getDescription() { return fDescription; }
};


#endif
