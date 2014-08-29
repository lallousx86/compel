#ifndef EXPRESSION_EVALUATOR__INC__
#define EXPRESSION_EVALUATOR__INC__


#pragma warning (disable:4786)
#include <stack>
#include <string>

namespace ExpressionEvaluator
{

  enum
  {
    eval_ok = 0,
    eval_unbalanced,
    eval_invalidoperator,
    eval_invalidoperand,
    eval_evalerr
  };

  int calculateLong(std::string expr, long &r);
  int calculateDouble(std::string expr, double &r);
}

#endif