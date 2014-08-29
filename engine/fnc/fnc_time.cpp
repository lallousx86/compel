#include "fnc_time.h"
#include <time.h>
#include <windows.h>

const static char *s_ns_name = "time.";
const static char *s_getdatetime = "getdatetime";
const static char *s_gettickcount = "gettickcount";
const static char *s_rand = "rand";
const static char *s_randomize = "randomize";
const static char *s_year = "year";
const static char *s_month = "month";
const static char *s_day = "day";
const static char *s_monthname = "monthname";
const static char *s_dow = "dow";
const static char *s_minute = "minute";
const static char *s_second = "second";
const static char *s_hour = "hour";
const static char *s_millisec = "millisec";

const static char *s_monthnames[] = 
{
  "January", "February", "March", 
  "April", "May", "June", 
  "July", "August", "September", 
  "October", "November", "December"
};

const char *fnc_time::fnc_name = "time";

fnc_time::fnc_time(interpreter_t *interpreter, op_e op)
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

fnc_time::fnc_time(bool bUseFullNS)
{
  _bUseNameSpace = bUseFullNS;
  _op = op_unknown;

  // randomize
  execute_randomize();
}

parse_errors_e fnc_time::register_function(interpreter_t *interpreter)
{
  fnc_time *functions[] =
  {
    new fnc_time(interpreter, op_rand),
    new fnc_time(interpreter, op_randomize),
    new fnc_time(interpreter, op_getdatetime),
    new fnc_time(interpreter, op_gettickcount)
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

parse_errors_e fnc_time::execute_getdatetime()
{
  static const int PAR_VAR = 1;

  interpreter_t *_int = get_interpreter();
  object_t *obj = _int->get_object_at(PAR_VAR);
  if (obj == 0)
    return parse_error_variable_expected;

  // clear old attributes
  obj->clear_attributes();

  SYSTEMTIME st;
  ::GetLocalTime(&st);

  obj->insert_attribute(s_year, value_t(st.wYear));
  obj->insert_attribute(s_month, value_t(st.wMonth));
  obj->insert_attribute(s_day, value_t(st.wDay));
  obj->insert_attribute(s_hour, value_t(st.wHour));
  obj->insert_attribute(s_second, value_t(st.wSecond));
  obj->insert_attribute(s_minute, value_t(st.wMinute));
  obj->insert_attribute(s_millisec, value_t(st.wMilliseconds));
  obj->insert_attribute(s_dow, value_t(st.wDayOfWeek));
  obj->insert_attribute(s_monthname, value_t(s_monthnames[st.wMonth-1]));

  return parse_error_none;
}

/*!
\brief Returns the system's ticks count since it was started
*/
parse_errors_e fnc_time::execute_gettickcount()
{
  static const int PAR_VAR = 1;

  interpreter_t *_int = get_interpreter();
  value_t *val = _int->get_value_at(PAR_VAR);
  if (val == 0)
    return parse_error_variable_expected;

  val->set_int_value(::GetTickCount());

  return parse_error_none;
}

parse_errors_e fnc_time::execute_rand()
{
  static const int PAR_VAR = 1;

  interpreter_t *_int = get_interpreter();
  value_t *val = _int->get_value_at(PAR_VAR);
  if (val == 0)
    return parse_error_variable_expected;

  val->set_int_value(::rand());

  return parse_error_none;
}

parse_errors_e fnc_time::execute_randomize()
{
  ::srand((unsigned int)time(NULL));
  return parse_error_none;
}

parse_errors_e fnc_time::execute()
{
  switch (_op)
  {
  case op_gettickcount:
    return execute_gettickcount();
  case op_randomize:
    return execute_randomize();
  case op_rand:
    return execute_rand();
  case op_getdatetime:
    return execute_getdatetime();
  }
  return parse_error_function_expected;
}
