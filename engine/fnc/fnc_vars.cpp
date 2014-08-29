#include "fnc_vars.h"
#include "../obj/memory_object.h"
#include "../function_helper.h"
#include <sstream>

static const char
  *str_alias_symbol_redefinition = "You tried to create an alias that conflicts with another name",
  *s_str_var = "var",
  *s_str_unvar = "unvar",
  *s_str_findvar = "findvar";

const char *fnc_vars::fnc_name = "vars";

parse_errors_e fnc_vars::register_function(interpreter_t *interpreter)
{
  fnc_vars *functions[] =
  {
    new fnc_vars(interpreter, vo_assign),
    new fnc_vars(interpreter, vo_var),
    new fnc_vars(interpreter, vo_findvar),
    new fnc_vars(interpreter, vo_alias),
    new fnc_vars(interpreter, vo_unvar)
  };

  for (size_t i=0;i<sizeof(functions)/sizeof(functions[0]);i++)
    interpreter->add_symbol(functions[i]->_name.c_str(), functions[i]);

  return parse_error_none;
}

fnc_vars::fnc_vars(interpreter_t *interpreter, vars_op op)
{
  _op = op;
  set_interpreter(interpreter);

  switch (_op)
  {
  case vo_var:
    set_namedesc(s_str_var, "$varname $varn [initvalue]<- declares variables");
    set_minmaxargs(1, 0);
    break;
  case vo_unvar:
    set_namedesc(s_str_unvar, "deletes a variable");
    set_minmaxargs(1, 1);
    break;
  case vo_alias:
    set_namedesc("alias", "creates an alias for a given variable. optionally removes the previous name");
    set_minmaxargs(2, 3);
    break;
  case vo_assign:
    set_namedesc("assign", "assigns a value to a given variable");
    set_minmaxargs(2, 0);
    break;
  case vo_findvar:
    set_namedesc(s_str_findvar, "creates an alias of a variable name expression");
    set_minmaxargs(2, 0);
    break;
  }
}

parse_errors_e fnc_vars::execute()
{
  switch (_op)
  {
  case vo_var:
    return execute_var();
  case vo_unvar:
    return execute_unvar();
  case vo_findvar:
    return execute_findvar();
  case vo_alias:
    return execute_alias();
  case vo_assign:
    //return execute_assign();
    return execute_var();
  }
  return parse_error_function_expected;
}

parse_errors_e fnc_vars::execute_findvar()
{
  /*
  const int PAR_NEWVAR = 1;
  const int PAR_EXPR = 2;

  interpreter_t *_int = get_interpreter();

  // get the variables names
  const char *newvarname = _int->get_const_at(PAR_OLDVAR);

  std::string out;
  function_helper::eval_from_to(_int, PAR_EXPR, _int->get_fnc_arg_count(), out, false, 0);

  symbol_t *oldsym = _int->find_symbol(oldvarname);

  if (newsym != 0)
  {
    _int->set_msg_warning(str_alias_symbol_redefinition);
    return parse_error_symbol_redefinition;
  }

  if (_int->add_symbol(newvarname, oldsym) == 0)
    return parse_error_symbol_redefinition;
  */
  return parse_error_none;
}

parse_errors_e fnc_vars::execute_alias()
{
  const int PAR_OLDVAR = 1;
  const int PAR_NEWVAR = 2;
  const int PAR_RENAME = 3;

  interpreter_t *_int = get_interpreter();

  // get the variables names
  const char *oldvarname = _int->get_const_at(PAR_OLDVAR);
  const char *newvarname = _int->get_const_at(PAR_NEWVAR);

  symbol_t *oldsym = _int->find_symbol(oldvarname);
  symbol_t *newsym = _int->find_symbol(newvarname);

  if (newsym != 0)
  {
    _int->set_msg_warning(str_alias_symbol_redefinition);
    return parse_error_symbol_redefinition;
  }

  if (_int->add_symbol(newvarname, oldsym) == 0)
    return parse_error_symbol_redefinition;

  // rename requested? thus we should remove the old name
  if ((_int->get_fnc_arg_count()-1) == PAR_RENAME)
    _int->remove_symbol(oldvarname);

  return parse_error_none;
}

parse_errors_e fnc_vars::execute_var()
{
  const int PAR_VAR = 1;
  interpreter_t *_int = get_interpreter();

  variable_t *var;
  object_t *obj = 0;
  std::string out;

  // get the variable's name
  const char *varname = _int->get_const_at(PAR_VAR);
  if (!_int->is_variable_name(varname))
    return parse_error_variable_expected;

  out = varname;

  // attempt to create an object?
  if (out.at(out.size()-1) == _int->get_object_separator())
  {
    out.erase(out.size()-1);
    varname = out.c_str();
    symbol_t *sym = _int->find_symbol(varname);
    // symbol not found, good we will create a new object
    if (sym == 0)
    {
      obj = new object_t;
      _int->add_symbol(varname, obj);
      return parse_error_none;
    }

    if (sym->get_sym_kind() != symbol_variable)
      return parse_error_variable_expected;

    var = static_cast<variable_t *>(sym);
    if (var->get_var_kind() != variable_object)
      return parse_error_object_expected;

    obj = static_cast<object_t *>(var);
    if (obj->get_obj_kind() != object_base)
      return parse_error_object_expected;
    return parse_error_none;
  }


  value_t *newval = 0;
  var = _int->get_var_ref(varname, out);
  bool b_from_string = false;

  // variable name
  if (var == 0)
  {
    newval = new value_t;
    _int->add_symbol(varname, newval);
  }
  else
  {
    if (var->get_var_kind() == variable_value)
    {
      newval = static_cast<value_t *>(var);
    }
    else if (var->get_var_kind() == variable_object)
    {
      obj = static_cast<object_t *>(var);
      const char *c_out = out.c_str();
      if (
           (_int->is_variable_name(c_out) == false) 
           ||
           (obj != _int->get_variable_raw(out.c_str()))
         )
      {
        newval = obj->insert_attribute(out.c_str());
      }
      else
      {
        b_from_string = true;
      }
    }
    else
    {
      return parse_error_variable_expected;
    }
  }

  function_helper::eval_from_to(_int, 2, _int->get_fnc_arg_count(), out, false, 0);

  if (obj && b_from_string)
    obj->from_string(out);
  else
    newval->set_str_value(out.c_str());

  return parse_error_none;
}

fnc_vars::fnc_vars()
{
  _op = vo_unknown;
}

parse_errors_e fnc_vars::execute_unvar()
{
  const int PAR_VAR = 1;

  interpreter_t *_int = get_interpreter();

  // get the variable's name
  const char *varname = _int->get_const_at(PAR_VAR);
  if (!_int->is_variable_name(varname))
    return parse_error_variable_expected;

  // ;! consider "unvar" for base_objects also
  // ;! special objects: no!
  variable_t *var = _int->get_variable(varname);
  if (var == 0)
    return parse_error_variable_expected;

  if ( 
       (var->get_var_kind() == variable_value)
       ||
        ( 
         (var->get_var_kind() == variable_object) 
         && 
         (static_cast<object_t *>(var)->get_obj_kind() == object_base)
        )
     )
  {
    std::string s = varname;
    std::string::size_type pos = s.find(_int->get_object_separator());

    // removing an object or a variable
    if (pos == std::string::npos)
    {
      if (!_int->remove_symbol(varname))
        return parse_error_symbol_not_in_table;
    }
    else
    {
      // removing an object's attribute
      std::string attr = s.substr(pos+1);
      s.erase(pos);
      object_t *obj = static_cast<object_t *>(_int->find_symbol(s.c_str()));

      if (obj->get_obj_kind() != object_base)
        return parse_error_object_expected;

      obj->remove_attribute(attr.c_str());
    }
  }

  return parse_error_none;
}