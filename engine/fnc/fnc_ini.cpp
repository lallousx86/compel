#include "fnc_ini.h"
#include "function_helper.h"

const static char *s_ns_name = "ini.";

const static char 
  *s_readini = "readini",
  *s_writeini = "writeini";

const char *fnc_ini::fnc_name = "ini";

fnc_ini::fnc_ini(interpreter_t *interpreter, op_e op)
{
  set_interpreter(interpreter);
  _op = op;
  switch (_op)
  {
  case op_readini:
    set_namedesc(s_readini, "$section $key &$val");
    set_minmaxargs(4, 4);
    break;
  case op_writeini:
    set_namedesc(s_writeini, "$section $key $val");
    set_minmaxargs(4, 0);
    break;
  }
}

fnc_ini::fnc_ini(bool bUseFullNS)
{
  _op = op_unknown;
  _bUseNameSpace = bUseFullNS;
}

parse_errors_e fnc_ini::register_function(interpreter_t *interpreter)
{
  fnc_ini *functions[] =
  {
    new fnc_ini(interpreter, op_readini),
    new fnc_ini(interpreter, op_writeini)
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

parse_errors_e fnc_ini::do_rw(bool bRead)
{
  interpreter_t *_int = get_interpreter();

  std::string str_file    = _int->evaluate_at(1);
  std::string str_section = _int->evaluate_at(2);
  std::string str_key     = _int->evaluate_at(3);

  if (bRead)
  {
    value_t *val = _int->get_value_at(4);
    if (val == 0)
      return parse_error_variable_expected;

    char temp[3];

    DWORD sz = ::GetPrivateProfileStringA(str_section.c_str(), str_key.c_str(), "", temp, sizeof(temp), str_file.c_str());
    if (sz == 0)
    {
      val->set_str_value("");
      return parse_error_none;
    }

    char *buf = new char[sz + 2];
    
    ::GetPrivateProfileStringA(str_section.c_str(), str_key.c_str(), "", buf, sz+1, str_file.c_str());

    val->set_str_value(buf);

    delete [] buf;
  }
  else
  {
    std::string str_value;

    function_helper::eval_from_to(_int, 4, _int->get_fnc_arg_count(), str_value, false, 0);
    ::WritePrivateProfileStringA(
      str_section.c_str(), 
      str_key.c_str(), 
      str_value.c_str(), 
      str_file.c_str());
  }
  return parse_error_none;
}

parse_errors_e fnc_ini::execute()
{
  switch (_op)
  {
  case op_readini:
    return do_rw(true);
  case op_writeini:
    return do_rw(false);
  }
  return parse_error_function_expected;
}
