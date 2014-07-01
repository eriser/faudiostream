/*
  
  This file is part of the FloPoCo project
  developed by the Arenaire team at Ecole Normale Superieure de Lyon
  
  Author : Florent de Dinechin, Florent.de-Dinechin@insa-lyon.fr

  Initial software.
  Copyright © ENS-Lyon, INRIA, CNRS, UCBL, INSA,
  2008-2014.
  All rights reserved.

*/
#ifndef __FIXHORNEREVALUATOR_HPP
#define __FIXHORNEREVALUATOR_HPP
#include <vector>
#include <sstream>

#include "Operator.hpp"
#include "Table.hpp"
#include "DualTable.hpp"


namespace flopoco{


  class FixHornerEvaluator : public Operator
  {
  public:
    /** The constructor with manual control of all options
     * @param wE exponent size
     */

    FixHornerEvaluator(Target* target, 
											 int wX, 
											 int degree, 
											 vector<int> coeffMSB, 
											 vector<int> coeffLSB, 
											 bool signedCoeffs=true, 
											 map<string, double> inputDelays = emptyDelayMap);
    ~FixHornerEvaluator();
		

  private:
    int wX;                           /**< size of X in bits; X is unsigned, in [0,1) */
    int degree;                       /**< degree of the polynomial */
    vector<int> coeffMSB;                  /**< vector of MSB weights for each coefficient */
    vector<int> coeffLSB;                  /**< vector of LSB weights for each coefficient */
    bool signedCoeffs;                /**< if false, all the coeffs are unsigned and the operator may use unsigned arithmetc. 
					 Usually true unless known Taylor etc */
    vector<int> coeffSize;            /**< vector of the sizes of the coefficients, computed out of MSB and LSB. See FixConstant.hpp for the constant format */

  };

}
#endif
