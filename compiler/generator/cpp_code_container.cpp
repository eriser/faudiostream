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

/**********************************************************************
			- code_gen.h : generic code generator (projet FAUST) -


		Historique :
		-----------

***********************************************************************/
#include "cpp_code_container.hh"
#include "cpp_gpu_code_container.hh"
#include "Text.hh"
#include "floats.hh"
#include "loki/SafeFormat.h"

using namespace std;

extern bool gUIMacroSwitch;
extern int gVectorLoopVariant;
extern bool gOpenMPLoop;
extern bool gVectorSwitch;
extern bool gOpenCLSwitch;
extern bool gCUDASwitch;
extern bool gOpenMPSwitch;
extern bool gSchedulerSwitch;
extern bool gVectorSwitch;
extern bool gFunTaskSwitch;

extern map<Tree, set<Tree> > gMetaDataSet;
map <string, int> CPPInstVisitor::gGlobalTable;

CodeContainer* CPPCodeContainer::createInternalContainer(const string& name, int sub_container_type)
{
    return new CPPScalarCodeContainer(name, "", 0, 1, fOut, sub_container_type);
}

CodeContainer* CPPCodeContainer::createContainer(int numInputs, int numOutputs, ostream* dst)
{
    // FIXME: hack to enable new compiler
    return new CPPMRCodeContainer("mydsp", "dsp", numInputs, numOutputs, dst);

    CodeContainer* container;

    if (gOpenCLSwitch) {
        if (gFunTaskSwitch) {
            cerr << "ERROR : -fun not yet supported in OpenCL mode" << endl;
            exit(1);
        }
        if (gVectorSwitch) {
            container = new CPPOpenCLVectorCodeContainer("mydsp", "dsp", numInputs, numOutputs, dst);
        } else {
            container = new CPPOpenCLCodeContainer("mydsp", "dsp", numInputs, numOutputs, dst);
        }
    } else if (gCUDASwitch) {
        if (gFunTaskSwitch) {
            cerr << "ERROR : -fun not yet supported in CUDA mode" << endl;
            exit(1);
        }
        if (gVectorSwitch) {
            container = new CPPCUDAVectorCodeContainer("mydsp", "dsp", numInputs, numOutputs, dst);
        } else {
            container = new CPPCUDACodeContainer("mydsp", "dsp", numInputs, numOutputs, dst);
        }
    } else if (gOpenMPSwitch) {
        container = new CPPOpenMPCodeContainer("mydsp", "dsp", numInputs, numOutputs, dst);
    } else if (gSchedulerSwitch) {
        container = new CPPWorkStealingCodeContainer("mydsp", "dsp", numInputs, numOutputs, dst);
    } else if (gVectorSwitch) {
        container = new CPPVectorCodeContainer("mydsp", "dsp", numInputs, numOutputs, dst);
    } else {
        container = new CPPScalarCodeContainer("mydsp", "dsp", numInputs, numOutputs, dst, kInt);
    }

    return container;
}

/*
void CPPCodeContainer::produceInfoFunctions(int tabs, const string& classname, bool isVirtual)
{
    stringstream out;
    string virtualPrefix;
    if (isVirtual)
        virtualPrefix = "virtual ";

    // Input/Output method

    out << virtualPrefix;
    Loki::FPrintf(out, "int " + classname + "::getNumInputs() { return %d; }\n")(fNumInputs);
    out << virtualPrefix;
    Loki::FPrintf(out, "int " + classname + "::getNumOutputs() { return %d; }\n")(fNumOutputs);

    // Input Rates
    {
        stringstream block1, block2;

        out << endl << virtualPrefix;
        Loki::FPrintf(out, "int " + classname + "::getInputRate(int channel) {\n");
        block1 << "switch (channel) {" << endl;

        for (int i = 0; i != fNumInputs; ++i)
            Loki::FPrintf(block2, "case %d: return %d;\n")(i)(fInputRates[i]);

        block2 << "default: -1;\n";

        block1 << indent(block2.str(), 1) << "}" << endl;
        out << indent(block1.str(), 1) << "}" << endl;
    }

    // Output Rates
    {
        stringstream block1, block2;

        out << endl << virtualPrefix;
        Loki::FPrintf(out, "int " + classname + "::getOutputRate(int channel) {\n");
        block1 << "switch (channel) {" << endl;

        for (int i = 0; i != fNumOutputs; ++i)
            Loki::FPrintf(block2, "case %d: return %d;\n")(i)(fOutputRates[i]);

        block2 << "default: -1;\n";

        block1 << indent(block2.str(), 1) << "}" << endl;
        out << indent(block1.str(), 1) << "}" << endl;
    }
    *fOut << endl << indent(out.str(), tabs);
}
*/

//Not yet working because of gGlobalTable in CPPInstVisitor (so function can be declared only once...)
void CPPCodeContainer::produceInfoFunctions(int tabs, const string& classname, bool isvirtual)
{
    // Input/Output method
    fCodeProducer.Tab(tabs);
    generateGetInputs(subst("$0::getNumInputs", classname), isvirtual)->accept(&fCodeProducer);
    generateGetOutputs(subst("$0::getNumOutputs", classname), isvirtual)->accept(&fCodeProducer);

    // Input Rates
    fCodeProducer.Tab(tabs);
    generateGetInputRate(subst("$0::getInputRate", classname), isvirtual)->accept(&fCodeProducer);

    // Output Rates
    fCodeProducer.Tab(tabs);
    generateGetOutputRate(subst("$0::getOutputRate", classname), isvirtual)->accept(&fCodeProducer);
}

void CPPCodeContainer::produceMetadata(int tabs)
{
    tab(tabs, *fOut); *fOut   << "void static metadata(Meta* m) { ";

    for (map<Tree, set<Tree> >::iterator i = gMetaDataSet.begin(); i != gMetaDataSet.end(); i++) {
        if (i->first != tree("author")) {
            tab(tabs+1, *fOut); *fOut << "m->declare(\"" << *(i->first) << "\", " << **(i->second.begin()) << ");";
        } else {
            for (set<Tree>::iterator j = i->second.begin(); j != i->second.end(); j++) {
                if (j == i->second.begin()) {
                    tab(tabs+1, *fOut); *fOut << "m->declare(\"" << *(i->first) << "\", " << **j << ");";
                } else {
                    tab(tabs+1, *fOut); *fOut << "m->declare(\"" << "contributor" << "\", " << **j << ");";
                }
            }
        }
    }

    tab(tabs, *fOut); *fOut << "}" << endl;
}

void CPPCodeContainer::produceInit(int tabs)
{
    tab(tabs, *fOut); *fOut << "virtual void init(int samplingFreq) {";
        tab(tabs+1, *fOut); *fOut << "classInit(samplingFreq);";
        tab(tabs+1, *fOut); *fOut << "instanceInit(samplingFreq);";
    tab(tabs, *fOut); *fOut << "}";
}

void CPPCodeContainer::produceInternal()
{
    int n = 0;

    // Global declarations
    tab(n, *fOut);

    fCodeProducer.Tab(n);
    generateGlobalDeclarations(&fCodeProducer);

    tab(n, *fOut); *fOut << "class " << fKlassName << " {";

        tab(n+1, *fOut);

        if (gUIMacroSwitch) {
            tab(n, *fOut); *fOut << "  public:";
        } else {
            tab(n, *fOut); *fOut << "  private:";
        }
        tab(n+1, *fOut);
        tab(n+1, *fOut);

        // Fields
        fCodeProducer.Tab(n+1);
        generateDeclarations(&fCodeProducer);

    tab(n, *fOut); *fOut << "  public:";

        tab(n+1, *fOut);
        produceInfoFunctions(n+1, fKlassName, false);

        // Inits
        tab(n+1, *fOut); *fOut << "void instanceInit" << fKlassName << "(int samplingFreq) {";
            tab(n+2, *fOut);
            fCodeProducer.Tab(n+2);
            generateInit(&fCodeProducer);
        tab(n+1, *fOut); *fOut << "}";

        // Fill
        string counter = "count";
        tab(n+1, *fOut);
        if (fSubContainerType == kInt) {
            tab(n+1, *fOut); *fOut << "void fill" << fKlassName << subst("(int $0, int* output) {", counter);
        } else {
            tab(n+1, *fOut); *fOut << "void fill" << fKlassName << subst("(int $0, $1* output) {", counter, ifloat());
        }
        tab(n+2, *fOut);
        fCodeProducer.Tab(n+2);
        generateComputeBlock(&fCodeProducer);
        ForLoopInst* loop = fCurLoop->generateScalarLoop(counter);
        loop->accept(&fCodeProducer);
        tab(n+1, *fOut); *fOut << "}";

    tab(n, *fOut); *fOut << "};" << endl;

    // Memory methods (as globals)
    tab(n, *fOut); *fOut << fKlassName << "* " << "new" <<  fKlassName << "() { "
                        << "return (" << fKlassName << "*) new "<< fKlassName << "()"
                        << "; }";

    tab(n, *fOut); *fOut << "void " << "delete" << fKlassName << "(" << fKlassName << "* dsp) { "
                        << "delete dsp"
                        << "; }";
    tab(n, *fOut);
}

void CPPCodeContainer::produceClass()
{
    int n = 0;

    // Initialize "fSamplingFreq" with the "samplingFreq" parameter of the init function
    pushFrontInitMethod(InstBuilder::genStoreGlobalVar("fSamplingFreq", InstBuilder::genLoadFunArgsVar("samplingFreq")));

    // Libraries
    printLibrary(*fOut);
    printIncludeFile(*fOut);

    // Sub containers
    generateSubContainers();

    // Global declarations
    tab(n, *fOut);
    fCodeProducer.Tab(n);
    generateGlobalDeclarations(&fCodeProducer);

    tab(n, *fOut); *fOut << "class " << fKlassName << " : public " << fSuperKlassName << " {";

        tab(n+1, *fOut);

        if (gUIMacroSwitch) {
            tab(n, *fOut); *fOut << "  public:";
        } else {
            tab(n, *fOut); *fOut << "  private:";
        }
        tab(n+1, *fOut);

        // Fields
        fCodeProducer.Tab(n+1);
        tab(n+1, *fOut);
        generateDeclarations(&fCodeProducer);

    tab(n, *fOut); *fOut << "  public:";

        // Print metadata declaration
        tab(n+1, *fOut);
        produceMetadata(n+1);

        tab(n+1, *fOut); *fOut << "virtual ~" << fKlassName << "() {";
            tab(n+2, *fOut); *fOut << "destroy();";
        tab(n+1, *fOut); *fOut << "}" << endl;

        tab(n+1, *fOut); *fOut << "void destroy() {";
            tab(n+2, *fOut);
            fCodeProducer.Tab(n+2);
            generateDestroy(&fCodeProducer);

        tab(n+1, *fOut);  *fOut << "}";
        tab(n+1, *fOut);

        produceInfoFunctions(n+1, fKlassName, true);  // Inits

        tab(n+1, *fOut); *fOut << "static void classInit(int samplingFreq) {";
            tab(n+2, *fOut);
            fCodeProducer.Tab(n+2);
            generateStaticInit(&fCodeProducer);
        tab(n+1, *fOut); *fOut << "}";

        tab(n+1, *fOut);
        tab(n+1, *fOut); *fOut << "virtual void instanceInit(int samplingFreq) {";
            tab(n+2, *fOut);
            fCodeProducer.Tab(n+2);
            generateInit(&fCodeProducer);
        tab(n+1, *fOut); *fOut << "}";

        tab(n+1, *fOut);
        produceInit(n+1);

        // User interface
        tab(n+1, *fOut);
        tab(n+1, *fOut); *fOut << "virtual void buildUserInterface(UI* interface) {";
            tab(n+2, *fOut);
            fCodeProducer.Tab(n+2);
            generateUserInterface(&fCodeProducer);
        tab(n+1, *fOut); *fOut << "}";

        // Compute
        generateCompute(n);
        tab(n, *fOut);

        // Possibly generate separated functions
        fCodeProducer.Tab(n+1);
        tab(n+1, *fOut);
        generateComputeFunctions(&fCodeProducer);

    tab(n, *fOut); *fOut << "};\n" << endl;

    // Generate user interface macros if needed
	if (gUIMacroSwitch) {
		tab(n, *fOut); *fOut << "#ifdef FAUST_UIMACROS";
            tab(n+1, *fOut); *fOut << "#define FAUST_INPUTS " << fNumInputs;
            tab(n+1, *fOut); *fOut << "#define FAUST_OUTPUTS " << fNumOutputs;
            tab(n+1, *fOut); *fOut << "#define FAUST_ACTIVES " << fNumActives;
            tab(n+1, *fOut); *fOut << "#define FAUST_PASSIVES " << fNumPassives;
			printlines(n+1, fUIMacro, *fOut);
		tab(n, *fOut); *fOut << "#endif";
        tab(n, *fOut);
	}
}

// Scalar
CPPScalarCodeContainer::CPPScalarCodeContainer(const string& name, const string& super, int numInputs, int numOutputs, std::ostream* out, int sub_container_type)
    :CPPCodeContainer(name, super, numInputs, numOutputs, out)
{
    fSubContainerType = sub_container_type;
}

CPPScalarCodeContainer::~CPPScalarCodeContainer()
{}

void CPPScalarCodeContainer::generateCompute(int n)
{
    // Generates declaration
    tab(n+1, *fOut);
    tab(n+1, *fOut); *fOut << subst("virtual void compute(int $0, $1** inputs, $1** outputs) {", fFullCount, xfloat());
    tab(n+2, *fOut);
    fCodeProducer.Tab(n+2);

    // Generates local variables declaration and setup
    generateComputeBlock(&fCodeProducer);

    // Generates one single scalar loop
    ForLoopInst* loop = fCurLoop->generateScalarLoop(fFullCount);
    loop->accept(&fCodeProducer);

    tab(n+1, *fOut); *fOut << "}";
}

// Vector
CPPVectorCodeContainer::CPPVectorCodeContainer(const string& name, const string& super, int numInputs, int numOutputs, std::ostream* out)
    :VectorCodeContainer(numInputs, numOutputs), CPPCodeContainer(name, super, numInputs, numOutputs, out)
{}

CPPVectorCodeContainer::~CPPVectorCodeContainer()
{}

void CPPVectorCodeContainer::generateCompute(int n)
{
    // Generates declaration
    tab(n+1, *fOut);
    tab(n+1, *fOut); *fOut << subst("virtual void compute(int $0, $1** inputs, $1** outputs) {", fFullCount, xfloat());
    tab(n+2, *fOut);
    fCodeProducer.Tab(n+2);

    // Generates local variables declaration and setup
    generateComputeBlock(&fCodeProducer);

    // Generate it
    fDAGBlock->accept(&fCodeProducer);

    /*
    ScalVecDispatcherVisitor visitor(new CPPInstVisitor(fOut, n+2), new CPPVecAccelerateInstVisitor(fOut, n+2));
    block->accept(&visitor);
    */

    tab(n+1, *fOut); *fOut << "}";
}

// OpenMP
CPPOpenMPCodeContainer::CPPOpenMPCodeContainer(const string& name, const string& super, int numInputs, int numOutputs, std::ostream* out)
    :OpenMPCodeContainer(numInputs, numOutputs), CPPCodeContainer(name, super, numInputs, numOutputs, out)
{}

CPPOpenMPCodeContainer::~CPPOpenMPCodeContainer()
{}

void CPPOpenMPCodeContainer::generateCompute(int n)
{
    // Compute declaration

    tab(n+1, *fOut);
    tab(n+1, *fOut); *fOut << subst("virtual void compute(int $0, $1** inputs, $1** outputs) {", fFullCount, xfloat());
    tab(n+2, *fOut);
    fCodeProducer.Tab(n+2);

    // Generates local variables declaration and setup
    generateComputeBlock(&fCodeProducer);

    // Generate it
    fGlobalLoopBlock->accept(&fCodeProducer);

    tab(n+1, *fOut); *fOut << "}";
}

// Works stealing scheduler
CPPWorkStealingCodeContainer::CPPWorkStealingCodeContainer(const string& name, const string& super, int numInputs, int numOutputs, std::ostream* out)
    :WSSCodeContainer(numInputs, numOutputs, "this"), CPPCodeContainer(name, super, numInputs, numOutputs, out)
{}

CPPWorkStealingCodeContainer::~CPPWorkStealingCodeContainer()
{}

void CPPWorkStealingCodeContainer::produceClass()
{
    int n = 0;

    // Inherited method
    CPPCodeContainer::produceClass();

    //tab(n, *fOut);
    *fOut << "void computeThreadExternal(void* dsp, int num_thread) {";
        tab(n+1, *fOut); *fOut << "static_cast<" << fKlassName << "*>(dsp)->computeThread(num_thread);";
    tab(n, *fOut); *fOut << "}" << endl;
}

void CPPWorkStealingCodeContainer::generateCompute(int n)
{
    lclgraph dag;
    CodeLoop::sortGraph(fCurLoop, dag);
    computeForwardDAG(dag);

    // Compute "compute" declaration
    tab(n+1, *fOut);
    tab(n+1, *fOut); *fOut << subst("virtual void compute(int $0, $1** inputs, $1** outputs) {", fFullCount, xfloat());
    tab(n+2, *fOut);
    fCodeProducer.Tab(n+2);

    // Generates local variables declaration and setup
    generateComputeBlock(&fCodeProducer);

    tab(n+1, *fOut); *fOut << "}" << endl;

    // Generates "computeThread" code
    tab(n+1, *fOut); *fOut << "void computeThread(int num_thread) {";
    tab(n+2, *fOut);
    fCodeProducer.Tab(n+2);

    // Generate it
    assert(fThreadLoopBlock);
    fThreadLoopBlock->accept(&fCodeProducer);

    tab(n+1, *fOut); *fOut << "}";
}

// Vector
CPPMRCodeContainer::CPPMRCodeContainer(const string& name, const string& super, int numInputs, int numOutputs, std::ostream* out)
    : CPPCodeContainer(name, super, numInputs, numOutputs, out)
{
    fFullCount = "fullcount";
}

CPPMRCodeContainer::CPPMRCodeContainer(const string& name, const string& super, int numInputs, int numOutputs, std::ostream* out, int sub_container_type)
    : CPPCodeContainer(name, super, numInputs, numOutputs, out)
{
    fFullCount = "fullcount";
    fSubContainerType = sub_container_type;
}

void CPPMRCodeContainer::generateCompute(int n)
{
    // Generates declaration
    tab(n+1, *fOut);
    tab(n+1, *fOut); *fOut << subst("virtual void compute(int $0, $1** inputs, $1** outputs) {", fFullCount, xfloat());
    tab(n+2, *fOut);
    fCodeProducer.Tab(n+2);

    // Generates local variables declaration and setup
    generateComputeBlock(&fCodeProducer);

    // Generate it
    // FIXME: hack to enable MR code generation
    mrBlock->accept(&fCodeProducer);

    tab(n+1, *fOut); *fOut << "}";
}

StatementInst* CPPMRCodeContainer::generateDAGLoopVariant0(const string& counter)
{
    string index = "index";

    // Define result block
    BlockInst* block_res = InstBuilder::genBlockInst();

    // Declare the "index" variable outside the loop
    DeclareVarInst* index_dec = InstBuilder::genDecStackVar(index, InstBuilder::genBasicTyped(Typed::kInt));
    block_res->pushBackInst(index_dec);
    block_res->pushBackInst(InstBuilder::genLabelInst("// Main loop"));

    BlockInst* loop_code = InstBuilder::genBlockInst();

    // Generate local input/output access
    generateLocalInputs(loop_code);
    generateLocalOutputs(loop_code);

    // Generate : int count = 32;
    DeclareVarInst* count_dec1 = InstBuilder::genDecStackVar("count", InstBuilder::genBasicTyped(Typed::kInt), InstBuilder::genIntNumInst(gVecSize));
    loop_code->pushBackInst(count_dec1);

    // Generates the loop DAG
    generateDAGLoop(loop_code, count_dec1);

    // Generates the DAG enclosing loop
    StoreVarInst* loop_init = index_dec->store(InstBuilder::genIntNumInst(0));

    ValueInst* loop_end = InstBuilder::genBinopInst(kLE, index_dec->load(),
        InstBuilder::genSub(InstBuilder::genLoadFunArgsVar(counter), InstBuilder::genIntNumInst(gVecSize)));

    StoreVarInst* loop_increment = index_dec->store(InstBuilder::genAdd(index_dec->load(), gVecSize));

    StatementInst* loop = InstBuilder::genForLoopInst(loop_init, loop_end, loop_increment, loop_code);

    // Put loop in block_res
    block_res->pushBackInst(loop);

    // Remaining frames
    block_res->pushBackInst(InstBuilder::genLabelInst("// Remaining frames"));

    ValueInst* if_cond = InstBuilder::genLessThan(InstBuilder::genLoadStackVar(index), InstBuilder::genLoadFunArgsVar(counter));

    BlockInst* then_block = InstBuilder::genBlockInst();

    // Generate local input/output access
    generateLocalInputs(then_block);
    generateLocalOutputs(then_block);

    // Generate : int count = fullcount-index;
    DeclareVarInst* count_dec2 = InstBuilder::genDecStackVar("count", InstBuilder::genBasicTyped(Typed::kInt), InstBuilder::genBinopInst(kSub,
                                    InstBuilder::genLoadFunArgsVar(counter), InstBuilder::genLoadStackVar(index)));

    then_block->pushBackInst(count_dec2);

    // Generates the loop DAG
    generateDAGLoop(then_block, count_dec2);

    block_res->pushBackInst(InstBuilder::genIfInst(if_cond, then_block));
    return block_res;
}

void CPPMRCodeContainer::produceInternal()
{
    int n = 0;

    // Global declarations
    tab(n, *fOut);

    fCodeProducer.Tab(n);
    generateGlobalDeclarations(&fCodeProducer);

    tab(n, *fOut); *fOut << "class " << fKlassName << " {";

        tab(n+1, *fOut);

        if (gUIMacroSwitch) {
            tab(n, *fOut); *fOut << "  public:";
        } else {
            tab(n, *fOut); *fOut << "  private:";
        }
        tab(n+1, *fOut);
        tab(n+1, *fOut);

        // Fields
        fCodeProducer.Tab(n+1);
        generateDeclarations(&fCodeProducer);

    tab(n, *fOut); *fOut << "  public:";

        tab(n+1, *fOut);
        produceInfoFunctions(n+1, fKlassName, false);

        // Inits
        tab(n+1, *fOut); *fOut << "void instanceInit (int samplingFreq) {";
            tab(n+2, *fOut);
            fCodeProducer.Tab(n+2);
            generateInit(&fCodeProducer);
        tab(n+1, *fOut); *fOut << "}";

        // Fill
        string counter = "fullCount";
        tab(n+1, *fOut);
        string fillArguments;
        if (fSubContainerType == kInt)
            fillArguments = subst("int $0, int* output", counter);
        else
            fillArguments = subst("int $0, $1* output", counter, ifloat());

        tab(n+1, *fOut); *fOut << "void fill (" << fillArguments << ") {";

        tab(n+2, *fOut);
        fCodeProducer.Tab(n+2);
        generateComputeBlock(&fCodeProducer);

        StatementInst * block = generateDAGLoopVariant0(counter);
        block->accept(&fCodeProducer);
        tab(n+1, *fOut); *fOut << "}";

    tab(n, *fOut); *fOut << "};" << endl;

    // wrapper methods (as globals)
    tab(n, *fOut); *fOut << "static " << fKlassName << "* " << "new" <<  fKlassName << "() { "
                        << "return (" << fKlassName << "*) new "<< fKlassName << "()"
                        << "; }";

    tab(n, *fOut); *fOut << "static void delete" << fKlassName << "(" << fKlassName << "* dsp) { "
                        << "delete dsp"
                        << "; }";

    tab(n, *fOut); *fOut << "static void fill" << fKlassName << "(" << fKlassName << "* dsp, " << fillArguments << ") { "
                        << "dsp->fill(" << counter << ", output)"
                        << "; }";

    tab(n, *fOut); *fOut << "static void instanceInit" << fKlassName << "(" << fKlassName << "* dsp, float samplingFreq) { "
                        << "dsp->instanceInit(samplingFreq)"
                        << "; }";


    tab(n, *fOut);
}
