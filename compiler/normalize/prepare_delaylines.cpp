/************************************************************************
 ************************************************************************
    FAUST compiler
    Copyright (C) 2011 GRAME, Centre National de Creation Musicale
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

#include "prepare_delaylines.hh"
#include "symbol.hh"
#include "signals.hh"
#include "sigtyperules.hh"

static Sym maxDelayProperty = symbol("maxDelayProperty");
static Tree maxDelayKey = tree(maxDelayProperty);

int getMaxDelay(Tree delayline)
{
    Tree dummy;
    assert(isSigDelayLine(delayline, dummy));

    Tree delProp = delayline->getProperty(maxDelayKey);
    if (!delProp)
        return -1;
    else
        return tree2int(delProp);
}


static void updateMaxDelayProperty(Tree sig, int newMax)
{
    int currentMaxDelay = 0;

    Tree currentProperty = sig->getProperty(maxDelayKey);
    if (currentProperty)
        currentMaxDelay = tree2int(currentProperty);

    if (newMax > currentMaxDelay)
        sig->setProperty(maxDelayKey, tree(newMax));
}

static void mappingFunction(Tree sig)
{
    Tree delayline, size;
    if (isSigFixDelay(sig, delayline, size)) {
        int iSize;
        if (isNum(size))
            iSize = tree2int(size);
        else {
            iSize = ceil(getSigType(size)->getInterval().hi);
        }
        updateMaxDelayProperty(delayline, iSize);
    }
}

void delaylineAnnotation (Tree root)
{
    mappingFunction(root);

    vector<Tree> vsigs;
    getSubSignals (root, vsigs);

    for (vector<Tree>::iterator it = vsigs.begin(); it != vsigs.end(); ++it)
        delaylineAnnotation(*it);
}