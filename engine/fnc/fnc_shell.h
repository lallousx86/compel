#ifndef __FNC__SHELL_03292006__
#define __FNC__SHELL_03292006__

#include "../function.h"
#include "../interpreter.h"
#include "../symbol_table.h"

class fnc_shell : public interpreter_errors_i, public function_t
{
public:
  enum op_e
  {
    op_none  = 0,
    op_shell = 1
  };
private:
  op_e _op;
  bool _b_pass_unknown_commands;
  virtual parse_errors_e ie_on_parse_error(size_t lineno, parse_errors_e err);

  int do_shell(const char *cmd);

public:
  fnc_shell(interpreter_t *, op_e op, bool b_pass_unknown_commands);

  fnc_shell(bool b_pass_unknown_commands);
  virtual ~fnc_shell();

  virtual parse_errors_e execute();
  virtual parse_errors_e register_function(interpreter_t *);

  static const char *fnc_name;
};

#endif