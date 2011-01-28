#include <stdio.h>
#include <assert.h>
#include "ensure.hh"
#include "tlib.hh"
#include "signals.hh"
#include "sigprint.hh"
#include "ppsig.hh"
#include "simplify.hh"
#include "normalize.hh"
#include "sigorderrules.hh"
#include "sigtyperules.hh"
#include "simplifying_terms.hh"
#include <map>
#include <list>

#include "mterm.hh"
#include "aterm.hh"

#if 0
static void countAddTerm (map<Tree,Tree>& M, Tree t, bool invflag);
static void incTermCount (map<Tree,int>& M, Tree t, bool invflag);
static Tree buildPowTerm (Tree f, int q);
static Tree simplifyingReorganizingMul(Tree t1, Tree t2);
static Tree reorganizingMul(Tree k, Tree t);
static void factorizeAddTerm(map<Tree,Tree>& M);
#endif

#undef TRACE

/**
 * Compute the Add-Normal form of a term t.
 * \param t the term to be normalized
 * \return the normalized term
 */
Tree normalizeAddTerm(Tree t)
{
#ifdef TRACE
	cerr << "START normalizeAddTerm : " << ppsig(t) << endl;
#endif

	aterm A(t);
	//cerr << "ATERM IS : " << A << endl;
	mterm D = A.greatestDivisor();
	while (D.isNotZero() && D.complexity() > 0) {
		//cerr << "GREAT DIV : " << D << endl;
		A = A.factorize(D);
		D = A.greatestDivisor();
	}
	return A.normalizedTree();
}


/**
 * Compute the normal form of a 1-sample delay term s'.
 * The normalisation rules are :
 *     	0' -> 0 /// INACTIVATE dec07 bug recursion
 *     	(k*s)' -> k*s'
 *		(s/k)' -> s'/k
 * \param s the term to be delayed by 1 sample
 * \return the normalized term
 */
Tree normalizeDelay1Term(Tree s)
{
    Tree one = tree(1);
    typeAnnotation(one);
    assert(sigValidInterval(one));
    return normalizeFixedDelayTerm(s, one);
}

/**
 * Compute the normal form of a fixed delay term (s@d).
 * The normalisation rules are :
 *		s@0 -> s
 *     	N@d -> N, if N is a number
 *     	(k*s)@d -> k*(s@d)
 *		(s/k)@d -> (s@d)/k
 * 		(s@n)@m -> s@(n+m)
 * Note that the same rules can't be applied to
 * + et - becaue the value of the first d samples
 * would be wrong. We could also add delays such that
 * \param delayline used delayline
 * \param d the value of the delay
 * \return the normalized term
 */

Tree normalizeFixedDelayTerm(Tree delayline, Tree d)
{
	Tree s, x, y, r;
	int i;

    ensure(isSigDelayLine(delayline, s));

	if (isZero(d) && ! isProj(s, &i, r))
        return s;

	if (isNum(s))
        return s;

	if (isSigMul(s, x, y)) {
		if (getSigOrder(x) < 2) {
            Tree normDel = normalizeFixedDelayTerm(sigDelayLine(y), d);
            return simplifyingMul(x, normDel);
		} else if (getSigOrder(y) < 2) {
            Tree normDel = normalizeFixedDelayTerm(sigDelayLine(x), d);
            return simplifyingMul(y, normDel);
		}

	} else if (isSigDiv(s, x, y)) {

		if (getSigOrder(y) < 2) {
            Tree normDel = normalizeFixedDelayTerm(sigDelayLine(x), d);
            return simplifyingDiv(normDel, y);
		}

	} else if (isSigFixDelay(s, x, y)) {
		// (x@n)@m = x@(n+m)
        Tree addTerm = simplifyingAdd(d, y);
        assert(addTerm->getType() ? sigValidInterval(addTerm) : true);
		return normalizeFixedDelayTerm(x, addTerm);
	}

    Tree ret = sigFixDelay(s, d);
    Type delayType = sampCast(s->getType());
    ret->setType(delayType);
    ret->branch(0)->setType(delayType);

    /* sigFixDelay introduces a cast node. we need to ensure the type */
    Tree sNormalized = NULL, dNormalized = NULL;
    isSigFixDelay(ret, sNormalized, dNormalized);
    if (dNormalized->getType() == NULL) {
        Type dNormalizedType = intCast(d->getType());
        dNormalized->setType(dNormalizedType);
    }

    return ret;
}

