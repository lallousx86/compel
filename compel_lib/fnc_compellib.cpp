#include "fnc_compellib.h"

const static char *s_ns_name = "compellib.";
const static char *s_dl = "dl";

const char *fnc_compellib::fnc_name = "compellib";

fnc_compellib::fnc_compellib(interpreter_t *interpreter, op_e op)
{
  set_interpreter(interpreter);
  _op = op;
  switch (_op)
  {
  case op_dl:
    set_namedesc(s_dl, "extname extpath<- dynamically loads an external extension");
    set_minmaxargs(2, 2);
    break;
  }
}

fnc_compellib::fnc_compellib(compel_lib_script_t *lib)
{
  _lib = lib;
  _op = op_unknown;
}

parse_errors_e fnc_compellib::register_function(interpreter_t *interpreter)
{
  fnc_compellib *functions[] =
  {
    new fnc_compellib(interpreter, op_dl)
  };

  for (size_t i=0;i<sizeof(functions)/sizeof(functions[0]);i++)
  {
    functions[i]->_lib = _lib; // pass the "lib" instance to the child commands
    std::string fnc_name = functions[i]->_name;
    fnc_name = s_ns_name + fnc_name;
    interpreter->add_symbol(fnc_name.c_str(), functions[i]);
  }

  return parse_error_none;
}

parse_errors_e fnc_compellib::execute_dl()
{
  interpreter_t *_int = get_interpreter();
  std::string extname, extpath;
  
  compel_string_tokenizer_t::unescape_c_string(_int->get_const_at(1), extname);
  compel_string_tokenizer_t::unescape_c_string(_int->get_const_at(2), extpath);

  _lib->user_extension_load(extname.c_str(), extpath.c_str());
  return (parse_errors_e) compel_lib_to_engine_error(_lib->get_last_engine_error());
}

parse_errors_e fnc_compellib::execute()
{
  switch (_op)
  {
  case op_dl:
    return execute_dl();
  }
  return parse_error_function_expected;
}
