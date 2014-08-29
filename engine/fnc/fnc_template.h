#ifndef __FNC_TEMPLATE_09052006__
#define __FNC_TEMPLATE_09052006__

#include "../function.h"
#include "../interpreter.h"
#include "../symbol_table.h"

class fnc_template : public function_t
{
public:
  enum op_e
  {
    op_unknown,
    op_rand
  };
private:
  op_e _op;
  bool _bUseNameSpace;

  parse_errors_e execute_rand();

public:
  static const char *fnc_name;
  fnc_template(interpreter_t *, op_e);
  fnc_template(bool bUseNameSpace);
  parse_errors_e execute();
  parse_errors_e register_function(interpreter_t *);
};

#endif