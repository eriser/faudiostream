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

#include "instructions_compiler_base.hh"
#include "timing.hh"
#include "sigtyperules.hh"

using namespace std;

// globals

extern int gMaxCopyDelay;
extern bool gVectorSwitch;
extern int gVecSize;
extern bool gOpenCLSwitch;
extern bool gCUDASwitch;

std::ostream* Printable::fOut = &cout;

/*****************************************************************************
                        getFreshID
*****************************************************************************/

map<string, int> InstructionsCompilerBase::fIDCounters;

string InstructionsCompilerBase::getFreshID(const string& prefix)
{
    if (fIDCounters.find(prefix) == fIDCounters.end()) {
        fIDCounters[prefix] = 0;
    }
    int n = fIDCounters[prefix];
    fIDCounters[prefix] = n+1;
    return subst("$0$1", prefix, T(n));
}

/*****************************************************************************
                            prepare
*****************************************************************************/

// Taken form sharing.cpp

int InstructionsCompilerBase::getSharingCount(Tree sig)
{
    //cerr << "getSharingCount of : " << *sig << " = ";
    Tree c;
    if (getProperty(sig, fSharingKey, c)) {
        //cerr << c->node().getInt() << endl;
        return c->node().getInt();
    } else {
        //cerr << 0 << endl;
        return 0;
    }
}

void InstructionsCompilerBase::setSharingCount(Tree sig, int count)
{
    //cerr << "setSharingCount of : " << *sig << " <- " << count << endl;
    setProperty(sig, fSharingKey, tree(count));
}

void InstructionsCompilerBase::sharingAnalysis(Tree t)
{
    fSharingKey = shprkey(t);
    if (isList(t)) {
        while (isList(t)) {
            sharingAnnotation(kSamp, hd(t));
            t = tl(t);
        }
    } else {
        sharingAnnotation(kSamp, t);
    }
}

void InstructionsCompilerBase::sharingAnnotation(int vctxt, Tree sig)
{
    Tree    c, x, y, z;

    //cerr << "START sharing annotation of " << *sig << endl;
    int count = getSharingCount(sig);

    if (count > 0) {
        // it is not our first visit
        setSharingCount(sig, count+1);

    } else {
        // it is our first visit,
        int v = getSigType(sig)->variability();

        // check "time sharing" cases
        if (v < vctxt) {
            setSharingCount(sig, 2);    // time sharing occurence : slower expression in faster context
        } else {
            setSharingCount(sig, 1);    // regular occurence
        }

        if (isSigSelect3(sig,c,y,x,z)) {
            // make a special case for select3 implemented with real if
            // because the c expression will be used twice in the C++
            // translation
            sharingAnnotation(v, c);
            sharingAnnotation(v, c);
            sharingAnnotation(v, x);
            sharingAnnotation(v, y);
            sharingAnnotation(v, z);
        } else {
            // Annotate the sub signals
            vector<Tree> subsig;
            int n = getSubSignals(sig, subsig);
            if (n>0 && ! isSigGen(sig)) {
                for (int i=0; i<n; i++) sharingAnnotation(v, subsig[i]);
            }
        }
    }
    //cerr << "END sharing annotation of " << *sig << endl;
}

Tree InstructionsCompilerBase::prepare(Tree LS)
{
 startTiming("InstructionsCompilerBase::prepare");
    sharingAnalysis(LS);            // annotate LS with sharing count
    fOccMarkup.mark(LS);            // annotate LS with occurences analysis
 endTiming("InstructionsCompilerBase::prepare");
    return LS;
}

Tree InstructionsCompilerBase::prepare2(Tree L0)
{
 startTiming("InstructionsCompilerBase::prepare2");
    sharingAnalysis(L0);            // annotate L0 with sharing count
    fOccMarkup.mark(L0);            // annotate L0 with occurences analysis
 endTiming("InstructionsCompilerBase::prepare2");
    return L0;
}

void InstructionsCompilerBase::getTypedNames(::Type t, const string& prefix, Typed::VarType& ctype, string& vname)
{
    if (t->nature() == kInt) {
        ctype = Typed::kInt;
        vname = subst("i$0", getFreshID(prefix));
    } else {
        ctype = itfloat();
        vname = subst("f$0", getFreshID(prefix));
    }
}

/**
 * Test if a signal is already compiled
 * @param sig the signal expression to compile.
 * @param name the string representing the compiled expression.
 * @return true is already compiled
 */
bool InstructionsCompilerBase::getCompiledExpression(Tree sig, InstructionsCompilerBase::InstType& cexp)
{
    return fCompileProperty.get(sig, cexp);
}

/**
 * Set the string of a compiled expression is already compiled
 * @param sig the signal expression to compile.
 * @param cexp the string representing the compiled expression.
 * @return the cexp (for commodity)
 */
InstructionsCompilerBase::InstType InstructionsCompilerBase::setCompiledExpression(Tree sig, const InstType& cexp)
{
    fCompileProperty.set(sig, cexp);
    return cexp;
}

/*****************************************************************************
                        vector name property
*****************************************************************************/

/**
 * Set the vector name property of a signal, the name of the vector used to
 * store the previous values of the signal to implement a delay.
 * @param sig the signal expression.
 * @param vname the string representing the vector name.
 * @return true is already compiled
 */
void InstructionsCompilerBase::setVectorNameProperty(Tree sig, const string& vname)
{
    assert(vname.size() > 0);
    fVectorProperty.set(sig, vname);
}

/**
 * Get the vector name property of a signal, the name of the vector used to
 * store the previous values of the signal to implement a delay.
 * @param sig the signal expression.
 * @param vname the string where to store the vector name.
 * @return true if the signal has this property, false otherwise
 */

bool InstructionsCompilerBase::getVectorNameProperty(Tree sig, string& vname)
{
    return fVectorProperty.get(sig, vname);
}

void InstructionsCompilerBase::setTableNameProperty(Tree sig, const string& name)
{
    assert(name.size() > 0);
    fTableProperty.set(sig, name);
}

bool InstructionsCompilerBase::getTableNameProperty(Tree sig, string& name)
{
    return fTableProperty.get(sig, name);
}
