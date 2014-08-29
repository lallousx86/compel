#ifndef __FNC_COMPELLIB_05242006__
#define __FNC_COMPELLIB_05242006__

#include "interpreter.h"
#include "function.h"
#include "function_helper.h"
#include "compel_lib.h"
#include "compel_lib_script.h"

class fnc_compellib : public function_t
{
public:
  enum op_e
  {
    op_unknown,
    op_dl
  };
private:
  compel_script_t _compel_script;
  compel_lib_script_t *_lib;
  op_e _op;

  parse_errors_e execute_dl();
public:
  static const char *fnc_name;

  fnc_compellib(interpreter_t *, op_e op);
  fnc_compellib(compel_lib_script_t *);

  parse_errors_e execute();
  parse_errors_e register_function(interpreter_t *interpreter);
};

#endif