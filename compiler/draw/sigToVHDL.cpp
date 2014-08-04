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

#define VHDL_DEBUG 
#ifdef VHDL_DEBUG
#define DDEFAULT 0
#define DL0 "0"
#define DL1 "1"
#define DL2 "2"
#define DL3 "3"
#define DL4 "4"
#define DL5 "5"
#define debug(...)                                    \
	do{                                               \
		std::string p[] = { __VA_ARGS__ };            \
		int dLevel=DDEFAULT;                          \
		if (sizeof(p)/sizeof(p[0])==2)               \
			dLevel=std::strtol(p[1].c_str(),0,10);    \
		if (dLevel <= VHDL_DEBUG)					  \
		cout<<"	DEBUG"<<dLevel<<":"<<p[0]<<endl;	  \
	} while(0)
#else
#define debug(...)
#endif


#include <stdio.h>

#include <iostream>
#include <sstream>
#include <string>
#include <stdint.h>

#include "signals.hh"
#include "sigtype.hh"
#include "sigtyperules.hh"
#include "xtended.hh"

#include "sigToVHDL.hh"


using namespace std;
using namespace flopoco;

static void     recdraw(Tree sig, set<Tree>& drawn, list<Tree> *recoveryPoints, bool isGettingReco=true, queue < pair<Tree,Tree > > *nFq=NULL, map<Tree, int> *lvs=NULL, map<Tree, list<Tree> > *nFs=NULL );
//static string   nodeattr(Type t);
//static string   edgeattr(Type t);
static string		sigLabel(Tree sig);

static void getRecPts(Tree sig, set<Tree>& drawn, list<Tree> *recoveryPoints);
static void prepareOps(Tree sig, set<Tree>& drawn, list<Tree> *recoveryPoints, map<Tree,int> *levels, map<Tree, list<Tree> > *nFs);

/**
 * Produce synthtizable VHDL using the flopoco framework
 */
void sigToVHDL (Tree L, ofstream& fout)
{

//	int wE = 9;
//	int wF = 16;
	
	//Operator* op = new IntAdder(target, wF);
	//Operator* op2 = new IntConstMult(target, wF, wE);
	//op->outputVHDLToFile(fout);
	//op2->outputVHDLToFile(fout);
	//delete op;
//    set<Tree>   alreadyDrawn;

    //fout << "strict digraph loopgraph {\n"
    //     << "    rankdir=LR; node [fontsize=10];"
    //     << endl;
    //int out = 0;
//		debug("constructing dataflow",DL1);
//    while (isList(L)) {
        //recdraw(hd(L), alreadyDrawn, fout, isGettingReco, nodeFathers, levels);

    //    fout << "OUTPUT_" << out << "[color=\"red2\" style=\"filled\" fillcolor=\"pink\"];" << endl;
    //    fout << 'S' << hd(L) << " -> " << "OUTPUT_" << out++ << "[" << edgeattr(getCertifiedSigType(hd(L))) << "];" << endl;
		//ending output.
//        L = tl(L);
//    }

   // fout << "}" << endl;
    set<Tree>   alreadyDrawn;
	list<Tree> *recoveryPoints=new list<Tree>();
	map<Tree,int> *lvls=new map<Tree, int>();
	map<Tree ,list<Tree> > *nodeFathers = new map<Tree, list<Tree> >();
	
	//getting recovery points
	getRecPts(L, alreadyDrawn, recoveryPoints );

	//getting levels and node-father connections
	prepareOps(L, alreadyDrawn, recoveryPoints, lvls, nodeFathers);

	//find the max level (pipeline depth)
	int max_level=0;
	for (map<Tree,int>::iterator maxFind = lvls->begin(); maxFind!=lvls->end();maxFind++){
		if (maxFind->second > max_level)
			max_level=maxFind->second;
	}

	//refactor to a list of nodes by level
	vector< list<Tree> > *ppls = new vector< list<Tree> >(max_level+1);
	map<Tree, int>::iterator refactorIt;
	for (refactorIt=lvls->begin(); refactorIt!=lvls->end(); ++refactorIt)
	{
		(*ppls)[refactorIt->second].push_back(refactorIt->first);
	}
	//rewriting missing proj at the appropriate level
	for (refactorIt=lvls->begin(); refactorIt!=lvls->end(); ++refactorIt)
	{
		//if a node is Proj, it might have many fathers but the hardware operation will
		//be reading in a memory so it's level is less than the highest (highest number) level and must be duplicated.
		int i;
		Tree t;
		if (isProj( refactorIt->first, &i, t ) ){ //if is proj
			vector<bool> isSaved(max_level+1); //know if we already put the proj on the lower levels
			//search all fathers in the levels
			for (list<Tree>::iterator father=(*nodeFathers)[refactorIt->first].begin();father!=(*nodeFathers)[refactorIt->first].end();father++){
				//if the level was not recorded. Keep in mind that it has already been recorded comming from the
				//nearest father, that is at level -1 from the current proj.
				if( ((*lvls)[*father]<(*lvls)[refactorIt->first]-1) && (!isSaved[refactorIt->second]) ){
					(*ppls)[(*lvls)[*father]+1].push_back(refactorIt->first);//record the proj.
					isSaved[refactorIt->second]=true; //update levels status.
				}
			}
		}
	}

	//output levels
	fout<<"Levels:"<<endl;
	for ( int i=0; i<ppls->size(); i++ )
	{
			fout<<i<<":";
		for ( list<Tree>::iterator debIt=(*ppls)[i].begin(); debIt!=(*ppls)[i].end(); debIt++)
		{
			fout<<"S"<<*debIt<<","<<sigLabel(*debIt)<<";";
		}
		fout<<endl;
	}

	//output node/father relations
	fout<<"\nConnections:"<<endl;
	for (map<Tree,list<Tree> >::iterator it=nodeFathers->begin(); it!=nodeFathers->end(); it++){
		if(it->second.size()!=0){
			fout <<"S"<<it->first <<":";
			for (list<Tree>::iterator i=it->second.begin();i!=it->second.end();i++)
			{
				fout << "S" <<*i<<";";
			}
			fout<<endl;
		}
	}
	debug("finished");

}


/******************************* IMPLEMENTATION ***********************************/


/**
 * Draw recursively a signal
 */
static void recdraw(Tree sig, set<Tree>& drawn, list<Tree> *recoveryPoints, bool isGettingReco, queue< pair <Tree, Tree > > *nFq, map<Tree,int> *lvs, map<Tree, list<Tree> > *nFs )
{
    //cerr << ++TABBER << "ENTER REC DRAW OF " << sig << "$" << *sig << endl;
    vector<Tree>    subsig;
    int             n;

    if (drawn.count(sig) == 0) {
        drawn.insert(sig);
        if (isList(sig)) {
            do {
                recdraw(hd(sig), drawn, recoveryPoints, isGettingReco, nFq, lvs, nFs);
                sig = tl(sig);
            } while (isList(sig));
        } else {

            // draw the node
     //       fout    << 'S' << sig << "[label=\"" << sigLabel(sig) << "\""
     //               << nodeattr(getCertifiedSigType(sig)) << "];"
     //               << endl;

            // draw the subsignals
            n = getSubSignals(sig, subsig);
            if (n > 0) {
                if (n==1 && isList(subsig[0])) {
					Tree id, body;
                    assert(isRec(sig,id,body));
					if (!isRec(sig,id,body)) {
					}

                    // special recursion case, recreate a vector of subsignals instead of the
                    // list provided by getSubSignal
					else
						recoveryPoints->push_back(sig);

                    Tree L = subsig[0];

                    subsig.clear();
                    n = 0;
                    do {
                        subsig.push_back(hd(L));
                        L = tl(L);
                        n += 1;
                    } while (isList(L));
                }


			}//FIXME:here
				if (isGettingReco){
					for (int i=0; i<n; i++) {
						recdraw(subsig[i], drawn, recoveryPoints, isGettingReco, nFq, lvs, nFs);
					}}
				else{
					
					int i;
					Tree x;
					if (isProj(sig, &i, x)){
						debug("Proj found",DL4);
						drawn.erase(sig);
					}

					if ( ( lvs->find(sig)==lvs->end() ) || !( (*lvs)[sig]==0 ) ){
						(*lvs)[sig]=((*lvs)[(*nFs)[sig].front()]+1); //record level of processed node as just higher than its father.
						}
					for (int i=0; i<n; i++) {
						nFq->push(pair<Tree, Tree>(subsig[i],sig));
						(*nFs)[subsig[i]].remove(sig); //if already exists, remove it and place it to front.
						(*nFs)[subsig[i]].push_front(sig);

						recdraw(subsig[i], drawn, recoveryPoints, isGettingReco, nFq, lvs, nFs);
					}
					

//                    fout    << 'S' << subsig[i] << " -> " << 'S' << sig
//                            << "[" << edgeattr(getCertifiedSigType(subsig[i])) << "];"
//                            << endl;
                }
		}
	}
}
    //cerr << --TABBER << "EXIT REC DRAW OF " << sig << endl;


static void getRecPts(Tree sig, set<Tree>& drawn, list<Tree> *recoveryPoints)
{
    set<Tree>   alreadyDrawn;

		debug("getting recovery points",DL1);
    while (isList(sig)) {
        recdraw(hd(sig), alreadyDrawn, recoveryPoints);

        sig = tl(sig);
    }
}
static void prepareOps( Tree sig, set<Tree>& drawn, list<Tree> *recoveryPoints, map<Tree,int> *levels, map<Tree, list<Tree> > *nFs )
{
    set<Tree> alreadyDrawn;
	queue < pair <Tree, Tree> > *nFq = new queue< pair<Tree,Tree> >();
	//map<Tree, int> *lvs = new map<Tree, int>();

	debug("getting recovery points",DL1);
    while (isList(sig)) {
		(*levels)[hd(sig)]=0;

		(*nFs)[hd(sig)]=list<Tree>(NULL);
		(*nFq).push(std::pair<Tree,Tree>(hd(sig),NULL));
        recdraw(hd(sig), alreadyDrawn,  recoveryPoints, false, nFq, levels, nFs);
        sig = tl(sig);
	}
}


/**
 * Convert a signal type into edge attributes
 */
//static string edgeattr(Type t)
//{
//    string s;
//
//    // nature
//    if (t->nature()==kInt) {
//        s += " color=\"blue\"";
//    } else {
//        s += " color=\"red\"";
//    }
//
//    // vectorability
//    if (t->vectorability()==kVect && t->variability()==kSamp) {
//        s += " style=\"bold\"";
//    }
//    return s;
//}


/**
 * Convert a signal type into node attributes
 */
//static string nodeattr(Type t)
//{
//    string s = edgeattr(t);
//
//    // variability
//    if (t->variability()==kKonst) {
//        s += " shape=\"box\"";
//    } else if (t->variability()==kBlock) {
//        s += " shape=\"hexagon\"";
//    } else if (t->variability()==kSamp) {
//        s += " shape=\"ellipse\"";
//    }
//
//    return s;
//}


/**
 * translate signal binary operations into strings
 */
static const char* binopname[]= {
        "+", "-", "*", "/", "%",
        "<<", ">>",
        ">", "<", ">=", "<=", "==", "!=",
        "&", "|", "^"
};


/**
 * return the label of a signal as a string
 */
static string sigLabel(Tree sig)
{
    int         i;
    double      r;
    Tree        x, y, z, c, type, name, file, ff, largs, id, le, sel, var, label;

    xtended*    p = (xtended*) getUserData(sig);

    stringstream fout;

         if (p)                                     { fout << p->name(); }
    else if ( isSigInt(sig, &i) )                   { fout << i;	}
    else if ( isSigReal(sig, &r) )                  { fout << r;	}
    else if ( isSigWaveform(sig))                   { fout << "waveform";  }

    else if ( isSigInput(sig, &i) )                 { fout << "INPUT_" << i; }
    else if ( isSigOutput(sig, &i, x) )             { fout << "OUTPUT_" << i; }

    else if ( isSigDelay1(sig, x) )                 { fout << "mem";		}
    else if ( isSigFixDelay(sig, x, y) )            { fout << "@";          }
    else if ( isSigPrefix(sig, x, y) )              { fout << "prefix";		}
    else if ( isSigIota(sig, x) )                   { fout << "iota";       }
    else if ( isSigBinOp(sig, &i, x, y) )           { fout << binopname[i]; }
    else if ( isSigFFun(sig, ff, largs) )			{ fout << "ffunction:" << *ff; }
    else if ( isSigFConst(sig, type, name, file) )  { fout << *name; }
    else if ( isSigFVar(sig, type, name, file) )    { fout << *name; }

    else if ( isSigTable(sig, id, x, y) ) 			{ fout << "table:" << id;	}
    else if ( isSigWRTbl(sig, id, x, y, z) )		{ fout << "write:" << id;	}
    else if ( isSigRDTbl(sig, x, y) ) 				{ fout << "read";	}



    else if ( isSigSelect2(sig, sel, x, y) ) 		{ fout << "select2"; }
    else if ( isSigSelect3(sig, sel, x, y, z) ) 	{ fout << "select3"; }

    else if ( isSigGen(sig, x) ) 					{ fout << "generator"; }

    else if ( isProj(sig, &i, x) )                  { fout << "Proj" << i;	}
    else if ( isRec(sig, var, le) )                 { fout << "REC " << *var; }

    else if ( isSigIntCast(sig, x) ) 				{ fout << "int"; }
    else if ( isSigFloatCast(sig, x) ) 				{ fout << "float"; }
#if 0
    else if ( isSigButton(sig, label) ) 			{ fout << "button \"" << *label << '"'; }
    else if ( isSigCheckbox(sig, label) ) 			{ fout << "checkbox \"" << *label << '"'; }
    else if ( isSigVSlider(sig, label,c,x,y,z) )	{ fout << "vslider \"" << *label << '"';  }
    else if ( isSigHSlider(sig, label,c,x,y,z) )	{ fout << "hslider \"" << *label << '"';  }
    else if ( isSigNumEntry(sig, label,c,x,y,z) )	{ fout << "nentry \"" << *label << '"';  }

    else if ( isSigVBargraph(sig, label,x,y,z) )	{ fout << "vbargraph \"" << *label << '"'; 	}
    else if ( isSigHBargraph(sig, label,x,y,z) )	{ fout << "hbargraph \"" << *label << '"'; 	}
#else
    else if ( isSigButton(sig, label) ) 			{ fout << "button"; }
    else if ( isSigCheckbox(sig, label) ) 			{ fout << "checkbox"; }
    else if ( isSigVSlider(sig, label,c,x,y,z) )	{ fout << "vslider";  }
    else if ( isSigHSlider(sig, label,c,x,y,z) )	{ fout << "hslider";  }
    else if ( isSigNumEntry(sig, label,c,x,y,z) )	{ fout << "nentry";  }

    else if ( isSigVBargraph(sig, label,x,y,z) )	{ fout << "vbargraph"; 	}
    else if ( isSigHBargraph(sig, label,x,y,z) )	{ fout << "hbargraph"; 	}
#endif
    else if ( isSigAttach(sig, x, y) )              { fout << "attach";		}

    else {
        cerr << "ERROR, unrecognized signal : " << *sig << endl;
        exit(1);
    }
    return fout.str();
}
