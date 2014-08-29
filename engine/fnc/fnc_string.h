#ifndef __FNC_STRING_05022006__
#define __FNC_STRING_05022006__

#include "../function.h"
#include "../interpreter.h"
#include "../symbol_table.h"

class fnc_string : public function_t
{
public:
  enum op_e
  {
    sc_none = 0,
    sc_tokenize,
    sc_strpos,
    sc_substr,
    sc_strlen,
  };

private:
  parse_errors_e execute_tokenize();
  parse_errors_e execute_strlen();
  parse_errors_e execute_strpos();
  parse_errors_e execute_substr();
  op_e _op;
  bool _b_use_namespace;

public:
  fnc_string(interpreter_t *, op_e op);

	static const char *fnc_name;

  fnc_string(bool b_use_namespace);

  parse_errors_e execute();
  parse_errors_e prepare(size_t passno, int &start_or_failing_line);
  parse_errors_e register_function(interpreter_t *);
};

#endif