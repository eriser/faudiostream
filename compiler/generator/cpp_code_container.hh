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

#ifndef _CPP_CODE_CONTAINER_H
#define _CPP_CODE_CONTAINER_H

/**********************************************************************
			- code_gen.h : generic code generator (projet FAUST) -


		Historique :
		-----------

***********************************************************************/
#include "code_container.hh"
#include "cpp_instructions.hh"
#include "opencl_instructions.hh"

#include "omp_code_container.hh"
#include "vec_code_container.hh"
#include "wss_code_container.hh"

extern string gMasterDocument;
extern string gOutputFile;

using namespace std;

class CPPCodeContainer : public virtual CodeContainer {

    protected:

        CPPInstVisitor fCodeProducer;
        std::ostream* fOut;
        string fKlassName;
        string fSuperKlassName;

        void produceInfoFunctions(int tabs, const string& classname, bool isVirtual);
        void produceMetadata(int tabs);
        void produceInit(int tabs);

    public:

        CPPCodeContainer(const string& name, const string& super, int numInputs, int numOutputs, std::ostream* out)
            : fCodeProducer(out), fOut(out), fKlassName(name), fSuperKlassName(super)
        {
            initializeCodeContainer(numInputs, numOutputs);
        }

        virtual void produceClass();
        virtual void generateCompute(int tab) = 0;
        virtual void produceInternal();

        CodeContainer* createScalarContainer(const string& name, int sub_container_type);

        static CodeContainer* createContainer(int numInputs, int numOutputs, ostream* dst);

};

class CPPScalarCodeContainer : public CPPCodeContainer {

    protected:

    public:

        CPPScalarCodeContainer(const string& name, const string& super, int numInputs, int numOutputs, std::ostream* out, int sub_container_type);
        virtual ~CPPScalarCodeContainer();

        void generateCompute(int tab);

};

class CPPVectorCodeContainer : public VectorCodeContainer, public CPPCodeContainer {

    protected:

    public:

        CPPVectorCodeContainer(const string& name, const string& super, int numInputs, int numOutputs, std::ostream* out);
        virtual ~CPPVectorCodeContainer();

        void generateCompute(int tab);

};

class CPPOpenMPCodeContainer : public OpenMPCodeContainer, public CPPCodeContainer {

    protected:

    public:

        CPPOpenMPCodeContainer(const string& name, const string& super, int numInputs, int numOutputs, std::ostream* out);
        virtual ~CPPOpenMPCodeContainer();

        void generateCompute(int tab);

};

class CPPWorkStealingCodeContainer : public WSSCodeContainer, public CPPCodeContainer {

    protected:

        void MoveStackSlow2Struct();

    public:

        CPPWorkStealingCodeContainer(const string& name, const string& super, int numInputs, int numOutputs, std::ostream* out);
        virtual ~CPPWorkStealingCodeContainer();

        void produceClass();
        void generateCompute(int tab);

};

class CPPMRCodeContainer : public CPPCodeContainer {
    public:
        BlockInst * mrBlock;

        CPPMRCodeContainer(const string& name, const string& super, int numInputs, int numOutputs, std::ostream* out);
        void generateCompute(int tab);

        void processFIR(void)
        {
            string index = "index";
            string counter = "cnt";

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

            mrBlock = block_res;
        }


};

#endif
