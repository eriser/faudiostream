#ifndef FixFunctionByTable_HPP
#define FixFunctionByTable_HPP
#include <vector>
#include <sstream>
#include <gmp.h>
#include <mpfr.h>
#include <gmpxx.h>

#include "../Table.hpp"
#include "FixFunction.hpp"

namespace flopoco{

	/** The FixFunctionByTable class */
	class FixFunctionByTable : public Table
	{
	public:
		/**
			 The FixFunctionByTable constructor. For the meaning of the parameters, see FixFunction.hpp
		 */

		FixFunctionByTable(Target* target, string func, int lsbIn, int msbOut, int lsbOut, int logicTable=1, map<string, double> inputDelays = emptyDelayMap);

		/**
		 * FixFunctionByTable destructor
		 */
		~FixFunctionByTable();
	
		mpz_class function(int x); // overloading Table method
		void emulate(TestCase * tc);


	protected:

		FixFunction *f;
		unsigned wR;
		
	};

}

#endif
