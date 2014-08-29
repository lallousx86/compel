#include "fnc_libusercommand.h"

fnc_libusercommand::fnc_libusercommand(interpreter_t *_int, p_lib_usercommand_info_t info)
{
  set_interpreter(_int);
  _info = *info;
}

parse_errors_e fnc_libusercommand::register_function(compel_script_t compel_script)
{
  interpreter_t *_int = get_interpreter();

  _compel_script = compel_script;

  // check the least parameters
  // - command name
  // - command callback
  if ((_info.name == 0) || (_info.cb == 0))
    return parse_error_function_expected;

  // register command return value variable
  _ret_name.clear();
  _ret_name.push_back(interpreter_t::get_variable_prefix());
  _ret_name += _info.name;

  // replace the '.' in the function name with '_'
  {
    using namespace std;
    string::size_type pos;
    while ((pos = _ret_name.find('.')) != string::npos)
      _ret_name[pos] = '_';
  }

  // either the command or its return value name are registered?
  if ( (_int->find_symbol(_info.name) != 0) || (_int->find_symbol(_ret_name.c_str()) != 0) )
    return parse_error_symbol_redefinition;

  set_minmaxargs(_info.minargs, _info.maxargs);
  set_namedesc(_info.name, _info.desc == 0 ? "" : _info.desc);

  // insert command
  if (_int->add_symbol(_info.name, this) == 0)
    return parse_error_symbol_redefinition;

  // insert command's return value
  value_t *val = new value_t;
  if (_int->add_symbol(_ret_name.c_str(), val) == 0)
  {
    delete val;
//    return parse_error_symbol_redefinition;
  }

  return parse_error_none;
}

parse_errors_e fnc_libusercommand::execute()
{
  interpreter_t *_int = get_interpreter();

  // get all arguments
  std::string out;
  function_helper::eval_from_to(_int, 1, _int->get_fnc_arg_count(), out, true);

  // replace all the '&$' with '$'
  {
    std::string::size_type pos;
    while ((pos = out.find("&$")) != std::string::npos)
      out.erase(pos, 1);
  }

  compel_string_tokenizer_t tok;
  size_t pcount = tok.parse(out.c_str());

  // Get argument restrictions
  size_t 
    minargs = get_minargs(), 
    maxargs = get_maxargs();

  // Check function min/max parameters
  if (minargs != 0 && pcount < minargs)
    return parse_error_less_param;
  else if (maxargs != 0  && pcount > maxargs)
    return parse_error_more_param;

  char **args = new char *[pcount];

  for (size_t i=0;i<pcount;i++)
    args[i] = (char *) tok.get_string(i);

  int err = _info.cb(_compel_script, (int) pcount, args);

  delete [] args;

  return (parse_errors_e) compel_lib_to_engine_error(err);
}

std::string &fnc_libusercommand::get_ret_name()
{
  return _ret_name;
}

p_lib_usercommand_info_t fnc_libusercommand::get_info()
{
  return &_info;
}