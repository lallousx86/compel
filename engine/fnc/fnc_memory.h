#ifndef  _FNC_MEMORY_01242006__
#define  _FNC_MEMORY_01242006__

#include "../fwd.h"
#include "../function.h"
#include "../interpreter.h"
#include "../symbol_table.h"

class fnc_memory : public function_t
{
public:
  enum memory_op
  {
    mo_malloc,
    mo_mfree
  };
private:
  memory_op _op;
  parse_errors_e execute_malloc();
  parse_errors_e execute_mfree();
public:
  fnc_memory(interpreter_t *, memory_op op);
  fnc_memory();
  parse_errors_e execute();
  parse_errors_e register_function(interpreter_t *);

  static const char *fnc_name;
};


#endif