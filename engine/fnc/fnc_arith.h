#ifndef __FNC_ARITH_01252006__
#define __FNC_ARITH_01252006__

#include "../function.h"
#include "../interpreter.h"
#include "../symbol_table.h"

class fnc_binary_arith : public function_t
{
public:
  enum binary_arith 
  { 
    ba_add = 0, 
    ba_sub, 
    ba_div, 
    ba_mul, 
    ba_expr
  };
private:
  binary_arith _op;
  bool _bUseNameSpace;

  parse_errors_e execute_expr();

public:
  fnc_binary_arith(interpreter_t *, binary_arith op);
  fnc_binary_arith(bool bUseNameSpace);
  parse_errors_e register_function(interpreter_t *);

  parse_errors_e execute();

  static const char *fnc_name;
};

#endif