#ifndef __FNC_TIME_09052006__
#define __FNC_TIME_09052006__

#include "../function.h"
#include "../interpreter.h"
#include "../symbol_table.h"

class fnc_time : public function_t
{
public:
  enum op_e
  {
    op_unknown,
    op_getdatetime,
    op_gettickcount,
    op_rand,
    op_randomize
  };
private:
  op_e _op;
  bool _bUseNameSpace;

  parse_errors_e execute_getdatetime();
  parse_errors_e execute_gettickcount();
  parse_errors_e execute_rand();
  parse_errors_e execute_randomize();

public:
  static const char *fnc_name;

  fnc_time(interpreter_t *, op_e);
  fnc_time(bool bUseNameSpace);
  parse_errors_e execute();
  parse_errors_e register_function(interpreter_t *);
};

#endif