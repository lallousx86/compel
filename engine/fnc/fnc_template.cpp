#include "fnc_template.h"

const static char *s_ns_name = "time.";
const static char *s_getdatetime = "getdatetime";
const static char *s_gettickcount = "gettickcount";
const static char *s_rand = "rand";
const static char *s_randomize = "randomize";

const char *fnc_template::fnc_name = "time";

fnc_template::fnc_template(interpreter_t *interpreter, op_e op)
{
  set_interpreter(interpreter);
  _op = op;
  switch (_op)
  {
  case op_getdatetime:
    set_namedesc(s_getdatetime, "$obj <- returns date/time");
    set_minmaxargs(1, 1);
    break;
  case op_rand:
    set_namedesc(s_rand, "$var <- returns a random integer");
    set_minmaxargs(1, 1);
    break;
  case op_randomize:
    set_namedesc(s_randomize, "randomizes the random number generator");
    set_minmaxargs(0, 0);
    break;
  case op_gettickcount:
    set_namedesc(s_gettickcount, "$var <- returns the system's ticks count");
    set_minmaxargs(1, 1);
    break;
  }
}

fnc_template::fnc_template(bool bUseFullNS)
{
  _bUseNameSpace = bUseFullNS;
}

parse_errors_e fnc_template::register_function(interpreter_t *interpreter)
{
  fnc_template *functions[] =
  {
    new fnc_template(interpreter, op_rand),
    new fnc_template(interpreter, op_randomize),
    new fnc_template(interpreter, op_getdatetime),
    new fnc_template(interpreter, op_gettickcount)
  };

  for (size_t i=0;i<sizeof(functions)/sizeof(functions[0]);i++)
  {
    std::string fnc_name = functions[i]->_name;
    if (_bUseNameSpace)
      fnc_name = s_ns_name + fnc_name;
    interpreter->add_symbol(fnc_name.c_str(), functions[i]);
  }
  return parse_error_none;
}

parse_errors_e fnc_template::execute_getdatetime()
{
  return parse_error_none;
}

parse_errors_e fnc_template::execute()
{
  switch (_op)
  {
  case op_gettickcount:
    return execute_gettickcount();
  case op_rand:
    return execute_rand();
  case op_getdatetime:
    return execute_gettickcount();
  }
  return parse_error_function_expected;
}
