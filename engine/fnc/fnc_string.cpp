#include "fnc_string.h"

const static char 
  *s_ns_name    = "string.",
  *s_cmd_strlen = "strlen",
  *s_cmd_tokenize = "tokenize",
  *s_cmd_substr = "substr",
  *s_cmd_strpos = "strpos",
  *s_str_delim = "delim",
  *s_str_str = "str",
  *s_str_count = "count";

const char *fnc_string::fnc_name = "string";

fnc_string::fnc_string(interpreter_t *interpreter, op_e op)
{
  _op = op;
  switch (_op)
  {
  case sc_tokenize:
    set_namedesc(s_cmd_tokenize, "<- $obj $str [$delim]");
    set_minmaxargs(2, 3);
    break;
  case sc_strpos:
    set_namedesc(s_cmd_strpos, "<- $pos $str $pattern");
    set_minmaxargs(3, 3);
    break;
  case sc_substr:
    set_namedesc(s_cmd_substr, "<- $substr $str $start $end");
    set_minmaxargs(4, 4);
    break;
  case sc_strlen:
    set_namedesc(s_cmd_strlen, "<- $str");
    set_minmaxargs(2, 2);
    break;
  }
  set_interpreter(interpreter);
}

parse_errors_e fnc_string::register_function(interpreter_t *interpreter)
{
  fnc_string *functions[] =
  {
    new fnc_string(interpreter, sc_tokenize),
    new fnc_string(interpreter, sc_substr),
    new fnc_string(interpreter, sc_strpos),
    new fnc_string(interpreter, sc_strlen)
  };

  for (size_t i=0;i<sizeof(functions)/sizeof(functions[0]);i++)
  {
    std::string fnc_name = functions[i]->_name;
    if (_b_use_namespace)
      fnc_name = s_ns_name + fnc_name;
    interpreter->add_symbol(fnc_name.c_str(), functions[i]);
  }
  return parse_error_none;
}

fnc_string::fnc_string(bool b_use_namespace)
{
  _op = sc_none;
  _b_use_namespace = b_use_namespace;
}

parse_errors_e fnc_string::execute()
{
  switch (_op)
  {
  case sc_tokenize:
    return execute_tokenize();
  case sc_strlen:
    return execute_strlen();
  case sc_strpos:
    return execute_strpos();
  case sc_substr:
    return execute_substr();
  }
  return parse_error_function_expected;
}

parse_errors_e fnc_string::execute_strlen()
{
  static const int PAR_STR    = 2;
  static const int PAR_LENVAR = 1;

  interpreter_t *_int = get_interpreter();

  const char *s = _int->get_const_at(PAR_LENVAR);
  if (!_int->is_variable_name(s))
    return parse_error_variable_expected;

  value_t *val = _int->get_value_at(PAR_LENVAR);
  if (val == 0)
    return parse_error_variable_expected;

  s = _int->evaluate_at(PAR_STR);

  val->set_int_value(strlen(s));

  return parse_error_none;
}

parse_errors_e fnc_string::execute_tokenize()
{
  static const int PAR_OBJ    = 1;
  static const int PAR_STR    = 2;
  static const int PAR_DELIM  = 3;

  interpreter_t *_int = get_interpreter();

  const char *objname = _int->get_const_at(PAR_OBJ);
  if (!_int->is_variable_name(objname))
    return parse_error_variable_expected;

  // Check if object exists
  object_t *obj = static_cast<object_t *>(_int->find_symbol(objname));
  if (obj == 0)
  {
    obj = new object_t;
    _int->add_symbol(objname, obj);
  }
  // Object should be base object
  else if (obj->get_obj_kind() != object_base)
  {
    return parse_error_symbol_type_mismatch;
  }
  // Clear the attributes of the objects and prepare them again
  else
  {
    obj->clear_attributes();
  }

  value_t val_temp;
  std::string str = _int->evaluate_at(PAR_STR);

  obj->insert_attribute(s_str_str, value_t(str.c_str()));

  std::string delim;
  
  if (_int->get_fnc_arg_count() > PAR_DELIM)
  {
    delim = _int->evaluate_at(PAR_DELIM);

    obj->insert_attribute(s_str_delim, value_t(delim.c_str()));

    compel_string_tokenizer_t tok;
    size_t pcnt = tok.parse(str.c_str(), delim.c_str());

    val_temp.set_int_value((long)pcnt);
    obj->insert_attribute(s_str_count, val_temp);

    for (size_t i=0;i<pcnt;i++)
    {
      char attr[15];
      sprintf(attr, "%d", i+1);
      val_temp.set_str_value(tok.get_string(i));
      obj->insert_attribute(attr, val_temp);
    }
  }
  // No delim was passed
  else
  {
    size_t len = str.size();
    obj->insert_attribute(s_str_count, value_t(len));
    obj->insert_attribute(s_str_delim, value_t(""));

    char chr_i[4] = {0};
    for (size_t i=0;i<len;i++)
    {
      char attr[15];
      chr_i[0] = str[i];
      sprintf(attr, "%d", i+1);
      val_temp.set_str_value(chr_i);
      obj->insert_attribute(attr, val_temp);
    }
  }
  return parse_error_none;
}

parse_errors_e fnc_string::execute_strpos()
{
  return parse_error_function_expected;
}

parse_errors_e fnc_string::execute_substr()
{
  return parse_error_function_expected;
}

parse_errors_e fnc_string::prepare(size_t passno, int &start_or_failing_line)
{
  return parse_error_none;
}
