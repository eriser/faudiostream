/************************************************************************
 ************************************************************************
    FAUST compiler
	Copyright (C) 2003-2004 GRAME, Centre National de Creation Musicale
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

#ifndef _INSTRUCTION_COMPILER_H
#define _INSTRUCTION_COMPILER_H

/**********************************************************************
			- code_gen.h : generic code generator (projet FAUST) -


		Historique :
		-----------

***********************************************************************/
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
#include "instructions_compiler_base.hh"

using namespace std;

extern string gMasterName;

typedef ValueInst* InstType;

class InstructionsCompiler:
    public InstructionsCompilerBase
{
    protected:
        Tree fUIRoot;
        bool fLoadedIota;

        StatementInst* generateInitArray(const string& vname, Typed::VarType ctype, int delay);
        StatementInst* generateCopyArray(const string& vname, int index_from, int index_to);
        StatementInst* generateCopyArray(const string& vname_to, const string& vname_from, int size);
        StatementInst* generateShiftArray(const string& vname, int delay);

        ValueInst* generateButtonAux(Tree sig, Tree path, const string& name);
        ValueInst* generateSliderAux(Tree sig, Tree path, Tree cur, Tree min, Tree max, Tree step, const string& name);
        ValueInst* generateBargraphAux(Tree sig, Tree path, Tree min, Tree max, ValueInst* exp, const string& name);

        void ensureIotaCode();

        CodeContainer* signal2Container(const string& name, Tree sig);

    public:

        InstructionsCompiler(CodeContainer* container)
            :InstructionsCompilerBase(container), fUIRoot(uiFolder(cons(tree(0),
            tree(subst("$0", gMasterName))))),
            fLoadedIota(false)
        {}

        virtual ValueInst* CS(Tree sig);

        virtual void compileMultiSignal(Tree sig);
        virtual void compileSingleSignal(Tree sig);

        virtual ValueInst* generateVariableStore(Tree sig, ValueInst* inst);
        virtual ValueInst* generateCacheCode(Tree sig, ValueInst* inst);

        // Code generation
        virtual ValueInst* generateCode(Tree sig);

        virtual ValueInst* generateXtended(Tree sig);
        virtual ValueInst* generateFixDelay(Tree sig, Tree arg, Tree size);
        virtual ValueInst* generatePrefix(Tree sig, Tree x, Tree e);
        virtual ValueInst* generateIota(Tree sig, Tree arg);
        virtual ValueInst* generateBinOp (Tree sig, int opcode, Tree arg1, Tree arg2);

        virtual ValueInst* generateFFun(Tree sig, Tree ff, Tree largs);

        virtual ValueInst* generateInput(Tree sig, int idx);

        virtual ValueInst* generateTable(Tree sig, Tree tsize, Tree content);
        virtual ValueInst* generateStaticTable(Tree sig, Tree tsize, Tree content);
        virtual ValueInst* generateWRTbl(Tree sig, Tree tbl, Tree idx, Tree data);
        virtual ValueInst* generateRDTbl(Tree sig, Tree tbl, Tree idx);
        virtual ValueInst* generateSigGen(Tree sig, Tree content);
        virtual ValueInst* generateStaticSigGen(Tree sig, Tree content);

        virtual ValueInst* generateSelect2(Tree sig, Tree sel, Tree s1, Tree s2);
        virtual ValueInst* generateSelect3(Tree sig, Tree sel, Tree s1, Tree s2, Tree s3);

        virtual ValueInst* generateRecProj(Tree sig, Tree exp, int i);
        virtual ValueInst* generateRec(Tree sig, Tree var, Tree le, int index);

        virtual ValueInst* generateIntCast(Tree sig, Tree x);
        virtual ValueInst* generateFloatCast(Tree sig, Tree x);

        virtual ValueInst* generateButton(Tree sig, Tree label);
        virtual ValueInst* generateCheckbox(Tree sig, Tree label);
        virtual ValueInst* generateVSlider(Tree sig, Tree label, Tree cur, Tree min, Tree max, Tree step);
        virtual ValueInst* generateHSlider(Tree sig, Tree label, Tree cur, Tree min, Tree max, Tree step);
        virtual ValueInst* generateNumEntry(Tree sig, Tree label, Tree cur, Tree min, Tree max, Tree step);

        virtual ValueInst* generateVBargraph(Tree sig, Tree label, Tree min, Tree max, ValueInst* exp);
        virtual ValueInst* generateHBargraph(Tree sig, Tree label, Tree min, Tree max, ValueInst* exp);

        virtual ValueInst* generateIntNumber(Tree sig, int num);
        virtual ValueInst* generateRealNumber(Tree sig, double num);
        virtual ValueInst* generateFConst(Tree sig, Tree type, const string& file, const string& name);
        virtual ValueInst* generateFVar(Tree sig, Tree type, const string& file, const string& name);

        virtual ValueInst* generateDelayVec(Tree sig, ValueInst* exp, Typed::VarType ctype, const string& vname, int mxd);
        virtual ValueInst* generateDelayLine(ValueInst* exp, Typed::VarType ctype, const string& vname, int mxd, Address::AccessType& var_access);

        // Gestion de la description arborescente de l'IU
        void addUIWidget(Tree path, Tree widget);
        Tree prepareUserInterfaceTree(Tree t);
        void generateUserInterfaceTree(Tree t);
        void generateUserInterfaceElements(Tree elements);
        void generateWidgetCode(Tree fulllabel, Tree varname, Tree sig);

        void generateMacroInterfaceTree(const string& pathname, Tree t);
        void generateMacroInterfaceElements(const string& pathname, Tree elements);
        void generateWidgetMacro(const string& pathname, Tree fulllabel, Tree varname, Tree sig);
};


#endif
