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

#define VHDL_DEBUG_
#ifdef VHDL_DEBUG_0
#define DDEFAULT 0
#define DL1 "1"
#define DL2 "2"
#define DL3 "3"
#define DL4 "4"
#define DL5 "5"
#define SPE ":SPECIAL"
#define debug(...)                                    \
	do{                                               \
		std::string p[] = { __VA_ARGS__ };            \
		if (sizeof(p)/sizeof(p[0])==2)                \
			cout<<"	DEBUG"<<p[1]<<":"<<p[0]<<endl;    \
		else                                          \
			cout<<"	DEBUG"<<DDEFAULT<<":"<<p[0]<<endl;\
	} while(0)
#else
#define debug(...)
#endif


#include <stdio.h>

#include <set>
#include <map>
#include <vector>
#include <iostream>
#include <sstream>
#include <string>

#include "signals.hh"
#include "sigtype.hh"
#include "sigtyperules.hh"
#include "xtended.hh"

#include "sigToVHDL.hh"

#include "flopoco/FloPoCo.hpp"

using namespace std;
using namespace flopoco;

static void     recdraw(Tree sig, set<Tree>& drawn, ofstream& fout );
//static string   nodeattr(Type t);
//static string   edgeattr(Type t);
static char		sigLabel(Tree sig, Operator *&op);

//replaces sigBase by sigSubst in the connection map.
static void		rpSig (void *sigBase, void *sigSubst);

static Target * target = new Virtex6();
static map<void*, Operator*> vhdlOps;
static multimap<void*,void*> connectMap;

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
    set<Tree>   alreadyDrawn;

    //fout << "strict digraph loopgraph {\n"
    //     << "    rankdir=LR; node [fontsize=10];"
    //     << endl;
    //int out = 0;
		debug("constructing dataflow");
    while (isList(L)) {
        recdraw(hd(L), alreadyDrawn, fout);

    //    fout << "OUTPUT_" << out << "[color=\"red2\" style=\"filled\" fillcolor=\"pink\"];" << endl;
    //    fout << 'S' << hd(L) << " -> " << "OUTPUT_" << out++ << "[" << edgeattr(getCertifiedSigType(hd(L))) << "];" << endl;
		//ending output.
        L = tl(L);
    }

	//output vhdl TODO: refactor, set a loop
	map<void*, Operator*>::iterator i,j;
	multimap<void*, void*>::iterator nextCo;
	map<string,Signal*> opSig, subOpSig;
	map<string,Signal*>::iterator sOp, sSub;
	Operator *opToDraw;

	for ( i=vhdlOps.begin(); i!=vhdlOps.end(); i++ )
	{
		debug("output vhdl for flopoco operator");
		opToDraw=i->second;
		opToDraw->outputVHDLToFile(fout);
		opSig=opToDraw->getSignalMap();
		
		debug("attempting to wire connections");
		nextCo=connectMap.begin();
		for
			(nextCo=connectMap.find(i->first);nextCo!=connectMap.end();nextCo=connectMap.find(i->first))
		{
			//wiring section TODO: refactor
			debug("getting connection", SPE);
			j=vhdlOps.find((*nextCo).second);
				if (j!=vhdlOps.end())
				{
				subOpSig=vhdlOps[(*nextCo).second]->getSignalMap();
			debug("got connection", SPE);
				//initiating double iterator research
				sSub=subOpSig.begin();
				for ( sOp = opSig.begin(); sOp != opSig.end(); sOp++){
					
			debug("testing connection", SPE);
					if ( sOp->second->type()==Signal::out )
						fout << sOp->first << "=>" << sSub->first
							<<endl;
			debug("wrote connection... ...Or not...", SPE);
					sSub++;
				}
		}
		else{}
			connectMap.erase(nextCo);
		debug("Erased current connection", SPE);
		//end of wiring section
		}


	}


	debug("deleting objects.");


	for ( i=vhdlOps.begin(); i!=vhdlOps.end(); i++ )
	{
		delete i->second;
	}

   // fout << "}" << endl;
}


/******************************* IMPLEMENTATION ***********************************/


/**
 * Draw recursively a signal
 */
static void recdraw(Tree sig, set<Tree>& drawn, ofstream& fout )
{
    //cerr << ++TABBER << "ENTER REC DRAW OF " << sig << "$" << *sig << endl;
    vector<Tree>    subsig;
    int             n;
	Operator* opFound;
	char isOp;
	stringstream opName;

    if (drawn.count(sig) == 0) {
        drawn.insert(sig);
        if (isList(sig)) {
            do {
                recdraw(hd(sig), drawn, fout);
                sig = tl(sig);
            } while (isList(sig));
        } else {

			debug("trying to get sigLabel", DL1);
			isOp=sigLabel(sig, opFound); //FIXME: rename this

			//changing name
			if (isOp)
			{
				opName << 'S' << sig; 
				opFound->changeName(opName.str());

				debug("inserting in vhdlOps", DL1);
				//recording op
				vhdlOps[sig]=opFound;
			}

            // draw the node
     //       fout    << 'S' << sig << "[label=\"" << sigLabel(sig) << "\""
     //               << nodeattr(getCertifiedSigType(sig)) << "];"
     //               << endl;

            // draw the subsignals
			debug("getting subsignals", DL1);
            n = getSubSignals(sig, subsig);
            if (n > 0) {
                if (n==1 && isList(subsig[0])) {
					Tree id, body;
                    assert(isRec(sig,id,body));
					if (!isRec(sig,id,body)) {
					}
                    // special recursion case, recreate a vector of subsignals instead of the
                    // list provided by getSubSignal
                    Tree L = subsig[0];
                    subsig.clear();
                    n = 0;
                    do {
                        subsig.push_back(hd(L));
                        L = tl(L);
                        n += 1;
                    } while (isList(L));
                }

			debug("generating connections", DL1);
				//TODO:two loops: one for recdraw and one for connection.
                for (int i=0; i<n; i++) {
                    recdraw(subsig[i], drawn, fout);
					
					if ( isOp ){
						connectMap.insert(std::pair<void*, void*>(sig,subsig[i]));
					}
					else {
						rpSig(sig, subsig[i]);
					}



					//portmap();

     //               fout    << 'S' << subsig[i] << " -> " << 'S' << sig
     //                       << "[" << edgeattr(getCertifiedSigType(subsig[i])) << "];"
     //                       << endl;
                }
            }
        }
    }
    //cerr << --TABBER << "EXIT REC DRAW OF " << sig << endl;
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
static char sigLabel(Tree sig, Operator *&op)
{
	char opf=0;
    int         i, wE=8, wF=24;
    double      r;
    Tree        x, y, z, c, type, name, file, ff, largs, id, le, sel, var, label;
	
    xtended*    p = (xtended*) getUserData(sig);

    stringstream fout;

         if (p)                                     { cout << p->name() <<" found."<<endl; }
    else if ( isSigInt(sig, &i) )                   { cout << i << " found."<<endl;	}
    else if ( isSigReal(sig, &r) )                  { cout << r << " found." <<endl;	}
    else if ( isSigWaveform(sig))                   { cout << "waveform" << " found." <<endl;  }

    else if ( isSigInput(sig, &i) )                 { cout << "INPUT_" << i << " found."<<endl; }
    else if ( isSigOutput(sig, &i, x) )             { cout << "OUTPUT_" << i<< " found."<<endl; }

    else if ( isSigDelay1(sig, x) )                 { cout << "mem" << " found." <<endl;		}
    else if ( isSigFixDelay(sig, x, y) )            { cout << "@" << " found."<<endl;          }
    else if ( isSigPrefix(sig, x, y) )              { cout << "prefix"<<" found."<< endl;		}
    else if ( isSigIota(sig, x) )                   { cout << "iota"<<" found.";       }
    else if ( isSigBinOp(sig, &i, x, y) )           { //fout << binopname[i]; }
		switch (i) {
			case 0: op = new FPAddSinglePath( target, wE, wF );
					opf=1;
					cout << "FPAddSinglePath found." << endl;
				break;
			case 1: op = new FPAddSinglePath ( target, wE, wF, true);
					cout << "FPSubSinglePath found." << endl;
					opf=1;
				break;
//			case 2:
//				break;
//			case 3:
//				break;
//			case 4:
//				break;
//			case 5:
//				break;
//			case 6:
//				break;
//			case 7:
//				break;
//			case 8:
//				break;
//			case 10:
//				break;
//			case 11:
//				break;
//			case 12:
//				break;
//			case 13:
//				break;
//			case 14:
//				break;
//			case 15:
//				break;
//			case 16:
//				break;
			default : cerr << "WARNING: " << binopname[i] << ":operation not supported yet. Operator will be skipped." << endl;
														}

													}
    else if ( isSigFFun(sig, ff, largs) )			{ cout << "ffunction:" << *ff << " found." <<endl; }
    else if ( isSigFConst(sig, type, name, file) )  { cout << *name << " found." <<endl; }
    else if ( isSigFVar(sig, type, name, file) )    { cout << *name << " found." << endl; }

    else if ( isSigTable(sig, id, x, y) ) 			{ cout << "table:" << id << " found." <<endl;	}
    else if ( isSigWRTbl(sig, id, x, y, z) )		{ cout << "write:" << id << " found." <<endl;	}
    else if ( isSigRDTbl(sig, x, y) ) 				{ cout << "read" << " found " << endl;	}



    else if ( isSigSelect2(sig, sel, x, y) ) 		{ cout << "select2" << " found." << endl; }
    else if ( isSigSelect3(sig, sel, x, y, z) ) 	{ cout << "select3" << " found." <<endl; }

    else if ( isSigGen(sig, x) ) 					{ cout << "generator" << " found."<<endl; }

    else if ( isProj(sig, &i, x) )                  { cout << "Proj" << i <<" found."<<endl;	}
    else if ( isRec(sig, var, le) )                 { cout << "REC " << *var << " found." <<endl; }

    else if ( isSigIntCast(sig, x) ) 				{ cout << "int" <<" found."<<endl; }
    else if ( isSigFloatCast(sig, x) ) 				{ cout << "float"<< " found" <<endl; }
#if 0
    else if ( isSigButton(sig, label) ) 			{ cout << "button \"" << *label << '"'; }
    else if ( isSigCheckbox(sig, label) ) 			{ cout << "checkbox \"" << *label << '"'; }
    else if ( isSigVSlider(sig, label,c,x,y,z) )	{ cout << "vslider \"" << *label << '"';  }
    else if ( isSigHSlider(sig, label,c,x,y,z) )	{ cout << "hslider \"" << *label << '"';  }
    else if ( isSigNumEntry(sig, label,c,x,y,z) )	{ cout << "nentry \"" << *label << '"';  }

    else if ( isSigVBargraph(sig, label,x,y,z) )	{ cout << "vbargraph \"" << *label << '"'; 	}
    else if ( isSigHBargraph(sig, label,x,y,z) )	{ cout << "hbargraph \"" << *label << '"'; 	}
#else
    else if ( isSigButton(sig, label) ) 			{ cout << "button" << " found."<<endl; }
    else if ( isSigCheckbox(sig, label) ) 			{ cout << "checkbox" << "found."<<endl; }
    else if ( isSigVSlider(sig, label,c,x,y,z) )	{ cout << "vslider" << "found."<<endl;}
    else if ( isSigHSlider(sig, label,c,x,y,z) )	{ cout << "hslider"  << "found."<<endl;}
    else if ( isSigNumEntry(sig, label,c,x,y,z) )	{ cout << "nentry"  << "found."<<endl; }

    else if ( isSigVBargraph(sig, label,x,y,z) )	{ cout << "vbargraph"  << "found."<<endl;	}
    else if ( isSigHBargraph(sig, label,x,y,z) )	{ cout << "hbargraph"  << "found."<<endl;	}
#endif
    else if ( isSigAttach(sig, x, y) )              { cout << "attach"	 << "found."<<endl;	}

    else {
        cerr << "ERROR, unrecognized signal : " << *sig << endl;
        exit(1);
    }

    return opf;
}

static void	rpSig (void *sigBase, void *sigSubst)
{
	multimap<void*, void*>::iterator i;
	for (i=connectMap.begin();i!=connectMap.end();i++)
	{
			debug("entering rpSig",DL2);
		if (i->second==sigBase)
		{	
			connectMap.insert(std::pair<void*,void*>(i->first, sigSubst));
			connectMap.erase(i);
			break;
		}
	}
			debug("exiting rpSig",DL2);
}
