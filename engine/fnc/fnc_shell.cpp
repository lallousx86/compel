#include "fnc_shell.h"
#include <process.h>
#include "../function_helper.h"

const char *fnc_shell::fnc_name = "shell";

fnc_shell::fnc_shell(interpreter_t *interpreter, op_e op, bool b_pass_unknown_commands)
{
  _op = op;
  set_interpreter(interpreter);
  _b_pass_unknown_commands = b_pass_unknown_commands;

  switch (_op)
  {
  case op_shell:
    set_namedesc("shell", "Passes commands to the system");
    set_minmaxargs(1, 0);

    // lowest priority.
    // we aim to be executed as last handler when all other handlers fail
    // we don't want to compete with other probably more important functions
    ie_set_priority(0); 
    if (_b_pass_unknown_commands)
      interpreter->error_client_register(this);
    break;
  }
}

int fnc_shell::do_shell(const char *cmd)
{
  return system(cmd);
}

parse_errors_e fnc_shell::ie_on_parse_error(size_t lineno, parse_errors_e err)
{
  interpreter_t *_int = get_interpreter();
  std::string out;
  function_helper::eval_from_to(_int, 0, _int->get_fnc_arg_count(), out, true, ' ');
  do_shell(out.c_str());
  return parse_error_none;
}

fnc_shell::fnc_shell(bool b_pass_unknown_commands)
{
  _op = op_none;
  _b_pass_unknown_commands = b_pass_unknown_commands;
}

fnc_shell::~fnc_shell()
{
  if (_op == op_shell && _b_pass_unknown_commands)
  {
    get_interpreter()->error_client_unregister(this);
  }
}

parse_errors_e fnc_shell::execute()
{
  interpreter_t *_int = get_interpreter();

  std::string out;
  function_helper::eval_from_to(_int, 1, _int->get_fnc_arg_count(), out, true, ' ');

  do_shell(out.c_str());

  return parse_error_none;
}

parse_errors_e fnc_shell::register_function(interpreter_t *interpreter)
{
  fnc_shell *functions[] =
  {
    new fnc_shell(interpreter, op_shell, _b_pass_unknown_commands)
  };

  for (size_t i=0;i<sizeof(functions)/sizeof(functions[0]);i++)
    interpreter->add_symbol(functions[i]->_name.c_str(), functions[i]);

	return parse_error_none;
}