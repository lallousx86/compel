#ifndef __FNC_COM_07052006__
#define __FNC_COM_07052006__

#include "../function.h"
#include "../interpreter.h"
#include "../symbol_table.h"

class fnc_com : public function_t
{
public:
  enum op_e
  {
    sc_none = 0,
    sc_create,
    sc_free,
    sc_invoke,
    sc_invokefast
  };
private:
  parse_errors_e execute_create();
  parse_errors_e execute_free();
  parse_errors_e execute_invoke();

  op_e _op;
public:
  fnc_com(interpreter_t *, op_e op);

	static const char *fnc_name;

  fnc_com();

  parse_errors_e execute();
  parse_errors_e prepare(size_t passno, int &start_or_failing_line);
  parse_errors_e register_function(interpreter_t *);
};

#endif