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

//================================= some string processing utilities =================================

/**
 * Removes enclosing whitespaces : '  toto  ' -> 'toto'
 */
static string wdel(const string& s)
{
    size_t i = 0;
    size_t j = s.size();
    while (i<j && s[i]==' ') i++;
    while (j>i && s[j-1] == ' ') j--;
    return s.substr(i,j-i);
}

//================================= BUILD USER INTERFACE METHOD =================================

/**
 * Generate buildUserInterface C++ lines of code corresponding
 * to user interface element t
 */
void InstructionsCompilerBase::generateUserInterfaceTree(Tree t)
{
    Tree label, elements, varname, sig;

    if (isUiFolder(t, label, elements)) {
        const int orient = tree2int(left(label));
        const char* str = tree2str(right(label));

        pushUserInterfaceMethod(InstBuilder::genOpenboxInst(orient, str));
        generateUserInterfaceElements(elements);
        pushUserInterfaceMethod(InstBuilder::genCloseboxInst());

    } else if (isUiWidget(t, label, varname, sig)) {

        generateWidgetCode(label, varname, sig);

    } else {

        fprintf(stderr, "error in user interface generation 2\n");
        exit(1);

    }
}

/**
 * Iterate generateUserInterfaceTree on a list of user interface elements
 */
void InstructionsCompilerBase::generateUserInterfaceElements(Tree elements)
{
    while (!isNil(elements)) {
        generateUserInterfaceTree(right(hd(elements)));
        elements = tl(elements);
    }
}

/**
 * Generate buildUserInterface C++ lines of code corresponding
 * to user interface widget t
 */
void InstructionsCompilerBase::generateWidgetCode(Tree fulllabel, Tree varname, Tree sig)
{
    Tree path, c, x, y, z;
    string label;
    map<string, set<string> > metadata;

    extractMetadata(tree2str(fulllabel), label, metadata);

    // Add metadata if any
    for (map<string, set<string> >::iterator i = metadata.begin(); i != metadata.end(); i++) {
        const string& key = i->first;
        const set<string>& values = i->second;
        for (set<string>::const_iterator j = values.begin(); j != values.end(); j++) {
            pushUserInterfaceMethod(InstBuilder::genAddMetaDeclareInst(tree2str(varname), wdel(key), wdel(*j)));
        }
    }

    if (isSigButton(sig, path))                     {
        pushUserInterfaceMethod(InstBuilder::genAddButtonInst(label, tree2str(varname)));

    } else if (isSigCheckbox(sig, path))            {
        pushUserInterfaceMethod(InstBuilder::genAddCheckbuttonInst(label, tree2str(varname)));

    } else if (isSigVSlider(sig, path,c,x,y,z)) {
        pushUserInterfaceMethod(
            InstBuilder::genAddVerticalSliderInst(label, tree2str(varname), tree2float(c), tree2float(x), tree2float(y), tree2float(z)));

    } else if (isSigHSlider(sig, path,c,x,y,z)) {
        pushUserInterfaceMethod(
            InstBuilder::genAddHorizontalSliderInst(label, tree2str(varname), tree2float(c), tree2float(x), tree2float(y), tree2float(z)));

    } else if (isSigNumEntry(sig, path,c,x,y,z))    {
        pushUserInterfaceMethod(
            InstBuilder::genAddNumEntryInst(label, tree2str(varname), tree2float(c), tree2float(x), tree2float(y), tree2float(z)));

    } else if (isSigVBargraph(sig, path,x,y,z)) {
        pushUserInterfaceMethod(
            InstBuilder::genAddVerticalBargraphInst(label, tree2str(varname),  tree2float(x), tree2float(y)));

    } else if (isSigHBargraph(sig, path,x,y,z)) {
        pushUserInterfaceMethod(
            InstBuilder::genAddHorizontalBargraphInst(label, tree2str(varname), tree2float(x), tree2float(y)));

    } else {
        fprintf(stderr, "Error in generating widget code\n");
        exit(1);
    }
}

//==================================== USER INTERFACE MACROS ==================================

/**
 * Generate user interface macros corresponding
 * to user interface element t
 */
void InstructionsCompilerBase::generateMacroInterfaceTree(const string& pathname, Tree t)
{
    Tree label, elements, varname, sig;

    if (isUiFolder(t, label, elements)) {
        string pathname2 = pathname;
        //string str = unquote(tree2str(right(label)));
        string str = tree2str(right(label));
        if (str.length() > 0) pathname2 += str + "/";
        generateMacroInterfaceElements(pathname2, elements);
    } else if (isUiWidget(t, label, varname, sig)) {
        generateWidgetMacro(pathname, label, varname, sig);
    } else {
        fprintf(stderr, "error in user interface macro generation 2\n");
        exit(1);
    }
}

/**
 * Iterate generateMacroInterfaceTree on a list of user interface elements
 */
void InstructionsCompilerBase::generateMacroInterfaceElements(const string& pathname, Tree elements)
{
    while (!isNil(elements)) {
        generateMacroInterfaceTree(pathname, right(hd(elements)));
        elements = tl(elements);
    }
}

/**
 * Generate user interface macros corresponding
 * to a user interface widget
 */
void InstructionsCompilerBase::generateWidgetMacro(const string& pathname, Tree fulllabel, Tree varname, Tree sig)
{
    Tree path, c, x, y, z;
    string label;
    map<string, set<string> >   metadata;

    extractMetadata(tree2str(fulllabel), label, metadata);

    //string pathlabel = pathname+unquote(label);
    string pathlabel = pathname+label;

    if (isSigButton(sig, path))                     {
        fContainer->addUIMacro(subst("FAUST_ADDBUTTON(\"$0\", $1);", pathlabel, tree2str(varname)));

    } else if (isSigCheckbox(sig, path))            {
        fContainer->addUIMacro(subst("FAUST_ADDCHECKBOX(\"$0\", $1);", pathlabel, tree2str(varname)));

    } else if (isSigVSlider(sig, path,c,x,y,z)) {
        fContainer->addUIMacro(subst("FAUST_ADDVERTICALSLIDER(\"$0\", $1, $2, $3, $4, $5);",
                pathlabel,
                tree2str(varname),
                T(tree2float(c)),
                T(tree2float(x)),
                T(tree2float(y)),
                T(tree2float(z))));

    } else if (isSigHSlider(sig, path,c,x,y,z)) {
        fContainer->addUIMacro(subst("FAUST_ADDHORIZONTALSLIDER(\"$0\", $1, $2, $3, $4, $5);",
                pathlabel,
                tree2str(varname),
                T(tree2float(c)),
                T(tree2float(x)),
                T(tree2float(y)),
                T(tree2float(z))));

    } else if (isSigNumEntry(sig, path,c,x,y,z))    {
        fContainer->addUIMacro(subst("FAUST_ADDNUMENTRY(\"$0\", $1, $2, $3, $4, $5);",
                pathlabel,
                tree2str(varname),
                T(tree2float(c)),
                T(tree2float(x)),
                T(tree2float(y)),
                T(tree2float(z))));

    } else if (isSigVBargraph(sig, path,x,y,z)) {
        fContainer->addUIMacro(subst("FAUST_ADDVERTICALBARGRAPH(\"$0\", $1, $2, $3);",
                pathlabel,
                tree2str(varname),
                T(tree2float(x)),
                T(tree2float(y))));

    } else if (isSigHBargraph(sig, path,x,y,z)) {
        fContainer->addUIMacro(subst("FAUST_ADDHORIZONTALBARGRAPH(\"$0\", $1, $2, $3);",
                pathlabel,
                tree2str(varname),
                T(tree2float(x)),
                T(tree2float(y))));

    } else {
        fprintf(stderr, "Error in generating widget code\n");
        exit(1);
    }
}


/**
 * Add a widget with a certain path to the user interface tree
 */
void InstructionsCompilerBase::addUIWidget(Tree path, Tree widget)
{
    fUIRoot = putSubFolder(fUIRoot, path, widget);
}

/**
 * Remove fake root folder if not needed (that is if the UI
 * is completely enclosed in one folder
 */
Tree InstructionsCompilerBase::prepareUserInterfaceTree(Tree t)
{
    Tree root, elems;
    if (isUiFolder(t, root, elems) && isList(elems) && isNil(tl(elems)) ) {
        Tree folder = right(hd(elems));
        return (isUiFolder(folder)) ? folder : t;
    }
    return t;
}
