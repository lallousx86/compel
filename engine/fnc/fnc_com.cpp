#include "fnc_com.h"
#include "../obj/com_object.h"
#include <set>
#include "../parse_util.h"

const static char 
  *s_crt_ns_name    = "com.",
  *s_cmd_create     = "create",
  *s_cmd_invoke     = "invoke",
  *s_cmd_free       = "free";

const char *fnc_com::fnc_name = "com";

fnc_com::fnc_com(interpreter_t *interpreter, op_e op)
{
  _op = op;
  switch (_op)
  {
  case sc_create:
    set_namedesc(s_cmd_create, "<- $com_obj $prog_id [clsctx]");
    set_minmaxargs(2, 3);
    break;
  case sc_free:
    set_namedesc(s_cmd_free, "<- $com_obj");
    set_minmaxargs(1, 1);
    break;
  case sc_invoke:
    set_namedesc(s_cmd_invoke, "<- $com_obj MethodName");
    set_minmaxargs(2, 2);
    break;
  }
  set_interpreter(interpreter);
}

fnc_com::fnc_com()
{
  _op = sc_none;
}

parse_errors_e fnc_com::execute_invoke()
{
  static const int PAR_OBJ = 1;
  static const int PAR_METHOD = 2;

  interpreter_t *_int = get_interpreter();

  // Is it a variable name?
  const char *objname = _int->get_const_at(PAR_OBJ);
  if (!_int->is_variable_name(objname))
    return parse_error_variable_expected;

  // Does the object exist?
  com_object_t *obj = dynamic_cast<com_object_t *>(_int->find_symbol(objname));
  if (obj == 0)
  {
    return parse_error_symbol_type_mismatch;
  }

  obj->invoke((char *)_int->evaluate_at(PAR_METHOD));

  return parse_error_none;
}

parse_errors_e fnc_com::execute_free()
{
  static const int PAR_OBJ = 1;

  interpreter_t *_int = get_interpreter();

  // Is it a variable name?
  const char *objname = _int->get_const_at(PAR_OBJ);
  if (!_int->is_variable_name(objname))
    return parse_error_variable_expected;

  // Does the object exist?
  com_object_t *obj = dynamic_cast<com_object_t *>(_int->find_symbol(objname));
  if (obj == 0)
  {
    return parse_error_symbol_type_mismatch;
  }

  _int->remove_symbol(objname);

  return parse_error_none;
}

parse_errors_e fnc_com::execute_create()
{
  static const int PAR_OBJ    = 1;
  static const int PAR_PROGID = 2;
  static const int PAR_CLSCTX = 3;

  interpreter_t *_int = get_interpreter();

  // Is it a variable name?
  const char *objname = _int->get_const_at(PAR_OBJ);
  if (!_int->is_variable_name(objname))
    return parse_error_variable_expected;

  // Does the object exist?
  object_t *obj = dynamic_cast<object_t *>(_int->find_symbol(objname));
  if (obj == 0)
  {
    obj = new com_object_t;
    _int->add_symbol(objname, obj);
  }
  // Correct object kind?
  else if (obj->get_obj_kind() != object_com)
  {
    return parse_error_symbol_type_mismatch;
  }

  // Get COM OBJECT
  com_object_t *comobj = dynamic_cast<com_object_t *>(obj);

  if (_int->get_fnc_arg_count() > PAR_CLSCTX)
  {
    comobj->_attr_clsctx->set_str_value(_int->evaluate_at(PAR_CLSCTX));
  }

  comobj->create((char *)_int->evaluate_at(PAR_PROGID));

  return parse_error_none;
}

parse_errors_e fnc_com::execute()
{
  switch (_op)
  {
  case sc_create:
    return execute_create();
  case sc_free:
    return execute_free();
  case sc_invoke:
    return execute_invoke();
  default:
    return parse_error_function_expected;
  }
}

parse_errors_e fnc_com::prepare(size_t passno, int &start_or_failing_line)
{
  return parse_error_none;
}

parse_errors_e fnc_com::register_function(interpreter_t *interpreter)
{
  fnc_com *functions[] =
  {
    new fnc_com(interpreter, sc_create),
    new fnc_com(interpreter, sc_free),
    new fnc_com(interpreter, sc_invoke)
  };

  for (size_t i=0;i<sizeof(functions)/sizeof(functions[0]);i++)
  {
    std::string fnc_name = functions[i]->_name;
    fnc_name = s_crt_ns_name + fnc_name;
    interpreter->add_symbol(fnc_name.c_str(), functions[i]);
  }
  return parse_error_none;
}
