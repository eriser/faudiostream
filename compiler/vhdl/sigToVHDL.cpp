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

#define VHDL_DEBUG 4
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

static void     fillOps(Tree sig, int level, map<Tree, int>& lvs, map<Tree, vector<Tree> >& nSs );
static string		sigLabel(Tree sig);

static void prepareOps(Tree sig, map<Tree,int>& levels, map<Tree, vector<Tree> >& nSs);

/**
 * Produce instructions to build vhdl pipeline with the flopoco framework
 */
void sigToVHDL (Tree L, ofstream& fout)
{
	map<Tree,int>			 lvls; // a collection of nodes with their level
	map<Tree ,vector<Tree> > nodeSons; // a collection of links between nodes and their sons
	int						 max_level=0; //max level in the pipeline
	int						 num; //int for proj testing
	Tree					 x, t, g; //Tree for proj/rec testing
	
	//getting levels and node-father connections
	prepareOps(L, lvls, nodeSons);

	//find the max level (pipeline depth)
	for (map<Tree,int>::iterator maxFind = lvls.begin(); maxFind!=lvls.end();maxFind++){
		if (maxFind->second > max_level)
			max_level=maxFind->second;
	}

	//refactor the list of levels by nodes to a list of nodes by level
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

			if (isProj(*debIt,&num,x))
				fout<<"S"<<*debIt<<",Proj"<<nodeSons[ nodeSons[*debIt][0] ][ num ]<<";"; //we take as identifier the provenance of the signal.
			//so we nodeSons[*debIt][0] is the first and only son of the proj node, so the rec node.
			//and we take the num-e son of this rec node.
			else if (isRec(*debIt, t, g)){ //special case for rec. Each son of rec will give birth to a different register.
				//so we output for each son a different rec node.
				for (unsigned int sons=0; sons<nodeSons[*debIt].size(); sons++){
					fout<<"S"<<*debIt<<"-"<<sons<<",Rec"<<nodeSons[*debIt][sons]<<";";
				}
			}

			else
				fout<<"S"<<*debIt<<","<<sigLabel(*debIt)<<";";
		}
		fout<<endl;
	}

	//output node/father relations
	fout<<"\nConnections:"<<endl;
	for (map<Tree,vector<Tree> >::iterator it=nodeSons.begin(); it!=nodeSons.end(); it++){
		if (isRec(it->first, t, g)){
			for (unsigned int sons=0; sons<nodeSons[it->first].size(); sons++){
				fout<<"S"<<it->first<<"-"<<sons<<":S"<<nodeSons[it->first][sons]<<";"<<endl;
			}
		}
		else if(it->second.size()!=0){
			fout <<"S"<<it->first <<":";
			for (vector<Tree>::iterator i=it->second.begin();i!=it->second.end();i++)
			{
				fout << "S" <<*i<<";";
			}
			fout<<endl;
		}
	}

	debug("finished");

}


/******************************* IMPLEMENTATION ***********************************/

/**prepares containers for displaying levels and tree father relationships*/
static void prepareOps( Tree sig, map<Tree,int>& t_levels, map<Tree, vector<Tree> >& nSs )
{

	debug("getting recovery points",DL1);
    while (isList(sig)) {
        fillOps(hd(sig), 0, t_levels, nSs);
        sig = tl(sig);
	}
}

/** fills levels and fathers links recursively*/
static void fillOps(Tree sig, int level, map<Tree,int>& tree_levels, map<Tree, vector<Tree> >& nSs )
{
    vector<Tree>    subsig;
    int             n;

	Tree trash, garbage;
	if ( isRec(sig,trash,garbage) ) //if is rec, reset the level
		level=0;

	if ( nSs.find(sig)==nSs.end() ){ //if not found
        if (isList(sig)) {
            do {
                fillOps(hd(sig), level, tree_levels, nSs);
                sig = tl(sig);
            } while (isList(sig));
        } else {

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

			//push parent relationship
				nSs[sig]=subsig;
			for (int i=0; i<n; i++) {

				fillOps(subsig[i], level+1, tree_levels, nSs);
				
			}
                
		}
	}



	if ( (tree_levels.find(sig)==tree_levels.end() ) || ( tree_levels.find(sig)->second<level ) ){
		tree_levels.erase(sig); //if already exists, and/or level is lower remove and replace with higher level.
		tree_levels.insert(pair<Tree, int>(sig, level));
		}
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
