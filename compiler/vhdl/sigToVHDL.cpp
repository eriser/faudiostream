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

#define VHDL_DEBUG 3
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
//usage: debug("string",DL1); //use defined DL1-5
//alternative: debug("string") //default debug level is 0


#include <stdio.h>

#include <iostream>
#include <sstream>
#include <string>
#include <stdint.h>

#include "signals.hh"
#include "sigtype.hh"
#include "sigtyperules.hh"
#include "xtended.hh"

#include "Text.hh"

#include "sigToVHDL.hh"

using namespace std;

static int		getTrueSubSigs(Tree sig, vector<Tree>& subsig);
static void		getNodeLevels(Tree sig, map<Tree,int>& levels);
static void     fillNodeLevels(Tree sig, int level, map<Tree, int>& lvs, int delay=0);
static string	sigLabel(Tree sig);


/**
 * Produce instructions to build vhdl pipeline with the flopoco framework
 */
void sigToVHDL (Tree L, ofstream& fout)
{
	map<Tree,int>			 lvls; // a collection of nodes with their level
	map<Tree ,vector<Tree> > nodeSons; // a collection of links between nodes and their sons
	int						 max_level=0; //max level in the pipeline
	int						 num,subsig_size; //int for proj testing
	Tree					 x, t, g; //Tree for proj/rec testing
	vector<Tree>			 subsig;
	
	//getting levels and node-father connections
	getNodeLevels(L, lvls);

	//find the max level (pipeline depth)
	for (map<Tree,int>::iterator maxFind = lvls.begin(); maxFind!=lvls.end();maxFind++){
		if (maxFind->second > max_level)
			max_level=maxFind->second;
	}

	//sort nodes by level
	vector< list<Tree> > ppls(max_level+1);
	map<Tree, int>::iterator refactorIt;
	for (refactorIt=lvls.begin(); refactorIt!=lvls.end(); refactorIt++)
	{
		ppls[refactorIt->second].push_back(refactorIt->first);
	}

	//output levels
	fout<<"Levels:"<<endl;
	for ( unsigned int i=0; i<ppls.size(); i++ )
	{
			fout<<i<<":";
		for ( list<Tree>::iterator debIt=ppls[i].begin(); debIt!=ppls[i].end(); debIt++)
		{

			if (isRec(*debIt, t, g)){ //special case for rec. Each son of rec will give birth to a different register.
				//so we output for each son a different rec node.
				getTrueSubSigs(*debIt,subsig);
				for (unsigned int sons=0; sons<subsig.size(); sons++){
					fout<<"S"<<*debIt<<"-"<<sons<<",Rec"<<";";
				}
				subsig.clear();
			}

			else
				fout<<"S"<<*debIt<<","<<sigLabel(*debIt)<<";";
		}
		fout<<endl;
	}


	//output node/son relations
	fout<<"\nConnections:"<<endl;
	for (map<Tree,int >::iterator it=lvls.begin(); it!=lvls.end(); it++){
		subsig_size=getTrueSubSigs( it->first, subsig );
		if ( isRec(it->first, t, g) ){
			for (unsigned int sons=0; sons<subsig.size(); sons++){
				fout<<"S"<<it->first<<"-"<<sons<<":S"<<subsig[sons]<<";"<<endl;
			}
		}
		else if ( isProj(it->first, &num, x) ){
			fout<<"S"<<it->first<<":S"<<subsig[0]<<"-"<<num<<endl;
		}
		else if( subsig_size ){
			fout <<"S"<<it->first <<":";
			for (vector<Tree>::iterator i=subsig.begin();i!=subsig.end();i++)
			{
				fout << "S" <<*i<<";";
			}
			fout<<endl;
		}
		subsig.clear();
	}
}


/******************************* IMPLEMENTATION ***********************************/

/**prepares and get node-levels associations*/
static void getNodeLevels( Tree sig, map<Tree,int>& t_levels )
{
    set<Tree>   alreadyDrawn;
    while (isList(sig)) {
        fillNodeLevels( hd(sig), 0, t_levels );
        sig = tl(sig);
	}
}

/** fills the collection of levels associated to nodes*/
static void fillNodeLevels( Tree sig, int level, map<Tree,int>& tree_levels, int delay )
{
    vector<Tree>    subsig;
    int             n,atDelay=-1,noi;
	Tree			atLeft,atRight,x;

	if ( (tree_levels.find(sig)==tree_levels.end() ) || ( tree_levels.find(sig)->second<level ) ){
		tree_levels.erase(sig); //if already exists, and/or level is lower remove and replace with higher level.
		tree_levels.insert(pair<Tree, int>(sig, level));

        if (isList(sig)) {
            do {
                fillNodeLevels(hd(sig), level, tree_levels);
                sig = tl(sig);
            } while (isList(sig));
        } else {

            // get all the subsignals
			n=getTrueSubSigs(sig, subsig);


		}
			//checking if delay
			if ( isSigFixDelay(sig, atLeft, atRight) ) {
				if ( !isSigInt(atLeft, &atDelay) ) {
					//if first son of FixDelay is an integer, get it in atDelay
					//else the second son has to be the delay, or we are in big trouble
					assert( isSigInt( atRight, &atDelay));
				}

				//recursively get levels of subsignals
				for (int i=0; i<n; i++) {
					fillNodeLevels(subsig[i], level+1, tree_levels, atDelay);
				}
			}
			//if proj and already recorded associated rec and delay is not null, do nothing
			//(don't need to re-record a node that will be used at one (or more) cycle of delay)
			else if (!( isProj(sig, &noi, x) && (tree_levels.find(subsig[0])!=tree_levels.end()) && delay )){
				//recursively get levels of subsignals
				for (int i=0; i<n; i++) {
					fillNodeLevels(subsig[i], level+1, tree_levels);
				}
			}
		}
}

/** gets real subsignals, avoiding rec nodes that contain
	a list of recursive signals (we only need the real subsignals */
static int getTrueSubSigs(Tree sig, vector<Tree>& subsig)
{
	int n;
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
	}
	return n;
}

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
    else if ( isSigReal(sig, &r) )                  { fout << T(r);	}
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

    else if ( isProj(sig, &i, x) )                  { fout << "Proj";	}
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
