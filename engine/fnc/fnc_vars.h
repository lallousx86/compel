#ifndef  _FNC_VARS_01242006__
#define  _FNC_VARS_01242006__

#include "../fwd.h"
#include "../function.h"
#include "../interpreter.h"
#include "../symbol_table.h"

class fnc_vars : public function_t
{
public:
  enum vars_op
  {
    vo_unknown,
    vo_var,
    vo_assign,
    vo_unvar,
    vo_alias,
    vo_findvar
  };
private:
  vars_op _op;

  // execute methods
  parse_errors_e execute_var();
  parse_errors_e execute_unvar();
  parse_errors_e execute_assign();
  parse_errors_e execute_alias();
  parse_errors_e execute_findvar();

public:
  fnc_vars(interpreter_t *, vars_op);
  parse_errors_e execute();
  fnc_vars();
  parse_errors_e register_function(interpreter_t *);

  static const char *fnc_name;
};


#endif