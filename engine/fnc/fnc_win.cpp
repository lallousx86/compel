#include "fnc_win.h"

const static char 
  *s_ns_name = "win.",
  *s_ismousedmoved = "ismousemoved",
  *s_getmousexy = "getmousexy";

const char *fnc_win::fnc_name = "win";

fnc_win::fnc_win(interpreter_t *interpreter, op_e op)
{
  set_interpreter(interpreter);
  _op = op;
  switch (_op)
  {
  case op_ismousemoved:
    set_namedesc(s_ismousedmoved, "$result <- returns if mouse was moved or not");
    set_minmaxargs(1, 1);
    break;
  case op_getmousexy:
    set_namedesc(s_getmousexy, "$x $y");
    set_minmaxargs(2, 2);
    break;
  }
}

fnc_win::fnc_win(bool bUseFullNS)
{
  _bUseNameSpace = bUseFullNS;
}

parse_errors_e fnc_win::register_function(interpreter_t *interpreter)
{
  fnc_win *functions[] =
  {
    new fnc_win(interpreter, op_ismousemoved),
    new fnc_win(interpreter, op_getmousexy)
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

parse_errors_e fnc_win::execute_ismousemoved()
{
  value_t *val = get_interpreter()->get_value_at(1);
  if (val == 0)
    return parse_error_variable_expected;

  static POINT last_pt = {-1, -1};
  POINT pt;

  ::GetCursorPos(&pt);

  if (last_pt.x == -1 && last_pt.y == -1)
  {
    val->set_int_value(0);
  }
  else
  {
    val->set_int_value(pt.x != last_pt.x || pt.y != last_pt.y);
  }
  last_pt = pt;
  return parse_error_none;
}

parse_errors_e fnc_win::execute_getmousexy()
{
  interpreter_t *_int = get_interpreter();
  
  value_t *v_x = _int->get_value_at(1);
  value_t *v_y = _int->get_value_at(2);
  
  if (v_x == 0 || v_y == 0)
    return parse_error_variable_expected;

  POINT pt;

  ::GetCursorPos(&pt);

  v_x->set_int_value(pt.x);
  v_y->set_int_value(pt.y);

  return parse_error_none;
}

parse_errors_e fnc_win::execute()
{
  switch (_op)
  {
  case op_ismousemoved:
    return execute_ismousemoved();
  case op_getmousexy:
    return execute_getmousexy();
  }
  return parse_error_function_expected;
}
